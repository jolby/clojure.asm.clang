(ns lisp.jit
  (:require [clojure.asm.llvm :refer :all])
  (:import (clojure.asm LLVMLibrary
                        LLVMLibrary$LLVMGenericValueRef
                        LLVMLibrary$LLVMExecutionEngineRef
                        LLVMLibrary$LLVMMCJITCompilerOptions
                        LLVMLibrary$LLVMMCJITMemoryManagerRef
                        LLVMLibrary$LLVMMemoryManagerAllocateCodeSectionCallback
                        LLVMLibrary$LLVMMemoryManagerAllocateDataSectionCallback
                        LLVMLibrary$LLVMMemoryManagerFinalizeMemoryCallback
                        LLVMLibrary$LLVMMemoryManagerDestroyCallback
                        LLVMLibrary$LLVMCodeModel)
           (com.sun.jna Memory Native Pointer)
           (com.sun.jna.ptr PointerByReference)
           (com.ochafik.lang.jnaerator.runtime NativeSize)
           (sun.misc Unsafe)))

(def ^:dynamic *section-head*)

(def get-page-size
  (memoize (fn []
             (-> (doto (.getDeclaredField Unsafe "theUnsafe")
                   (.setAccessible true))
                 (.get nil)
                 (.pageSize)))))

(defn allocate-code-section
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerAllocateCodeSectionCallback
    (apply [_ opaque size alignment section-id section-name]
      (f opaque size alignment section-id section-name))))

(defn allocate-data-section
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerAllocateDataSectionCallback
    (apply [_ opaque size alignment section-id section-name read-only?]
      (f opaque size alignment section-id section-name read-only?))))

(defn finalize-memory
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerFinalizeMemoryCallback
    (apply [_ opaque err-msg]
      (f opaque err-msg))))

(defn destroy
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerDestroyCallback
    (apply [_ opaque]
      (f opaque))))

(defn lisp-code-section-allocator
  [opaque size alignment section-id section-name]
  (let [page-size (get-page-size)
        start (Memory. (bit-and-not (+ size (dec page-size)) page-size))
        section {:start start
                 :size size
                 :next *section-head*}]
    (set! *section-head* section)
    start))

(defn lisp-data-section-allocator
  [opaque size alignment section-id section-name read-only?]
  (lisp-code-section-allocator opaque size alignment section-id section-name))

(defn lisp-memory-finalizer
  [opaque err-msg]
  true)

(defn lisp-memory-destroyer
  [opaque])

(defn lisp-memory-manager
  []
  (let [code-allocator (allocate-code-section lisp-code-section-allocator)
        data-allocator (allocate-data-section lisp-data-section-allocator)
        memory-finalizer (finalize-memory lisp-memory-finalizer)
        memory-destroyer (destroy lisp-memory-destroyer)]
    (LLVMCreateSimpleMCJITMemoryManager Pointer/NULL
                                        code-allocator
                                        data-allocator
                                        memory-finalizer
                                        memory-destroyer)))

(defn mcjit-compiler-options
  ([] (mcjit-compiler-options 0))
  ([opt-level]
     (mcjit-compiler-options opt-level 1))
  ([opt-level code-model]
     (mcjit-compiler-options opt-level code-model false))
  ([opt-level code-model no-fp-elim]
     (mcjit-compiler-options opt-level code-model no-fp-elim true))
  ([opt-level code-model no-fp-elim fast-isel]
     (mcjit-compiler-options 0 1 true true (lisp-memory-manager)))
  ([opt-level code-model no-fp-elim fast-isel memory-manager]
     (let [options (LLVMLibrary$LLVMMCJITCompilerOptions. opt-level
                                                          code-model
                                                          no-fp-elim
                                                          fast-isel
                                                          memory-manager)]
       (doto options
         (LLVMInitializeMCJITCompilerOptions (NativeSize. (.size options)))))))

(defn mcjit
  [module]
  (let [ee (PointerByReference.)
        options (mcjit-compiler-options)
        sizeof (NativeSize. (.size options))
        error (PointerByReference.)]
    (when-not (LLVMCreateMCJITCompilerForModule ee module options sizeof error)
      (LLVMLibrary$LLVMExecutionEngineRef. (.getValue ee)))))

(defn values
  [module]
  (take-while (complement nil?)
              (iterate LLVMGetNextFunction (LLVMGetFirstFunction module))))

(defn declarations
  [module]
  (filter LLVMIsDeclaration (values module)))

(defn functions
  [module]
  (remove LLVMIsDeclaration (values module)))

(defn compiled-functions
  [engine module]
  (map (partial LLVMGetPointerToGlobal engine) (functions module)))

(defn compile-module
  [module]
  (let [jit (mcjit module)
        passes (LLVMCreatePassManager)
        xs (transient [])]
    (LLVMAddTargetData (LLVMGetExecutionEngineTargetData jit) passes)
    (doto passes
      (LLVMAddPromoteMemoryToRegisterPass)
      (LLVMAddConstantPropagationPass)
      (LLVMAddBasicAliasAnalysisPass)
      (LLVMAddTypeBasedAliasAnalysisPass)
      (LLVMAddGVNPass)
      (LLVMAddCFGSimplificationPass)
      (LLVMAddTailCallEliminationPass))
    (LLVMRunPassManager passes module)
    (LLVMDumpModule module)
    (doseq [x (compiled-functions jit module)]
      (conj! xs x))
    ;; (LLVMDisposePassManager passes)
    ;; (LLVMDisposeExecutionEngine jit)
    (persistent! xs)))
