(ns lisp.compiler
  (:require [clojure.asm.llvm :refer :all]
            [lisp.jit :as jit]
            [criterium.core :refer [quick-bench]])
  (:import (clojure.asm LLVMLibrary
                        LLVMLibrary$LLVMTypeRef
                        LLVMLibrary$LLVMValueRef
                        LLVMLibrary$LLVMBasicBlockRef
                        LLVMLibrary$LLVMCallConv)
           (com.sun.jna Callback Function Library Platform Native Pointer)
           (org.bridj TypedPointer DynamicFunction)
           (org.bridj.ann Convention Convention$Style)
           (java.lang.reflect Type)))

(def fib-form
  '(fn ^int fib (^int n) n))

(defn emit
  [])

(defn types
  [& types]
  (into-array LLVMLibrary$LLVMTypeRef types))

(defn values
  [& values]
  (into-array LLVMLibrary$LLVMValueRef values))

(defn basic-blocks
  [& basic-blocks]
  (into-array LLVMLibrary$LLVMBasicBlockRef basic-blocks))

(defn function-type
  ([return-type param-types] (function-type return-type param-types false))
  ([return-type param-types variadic?]
     (LLVMFunctionType return-type (apply types param-types) (count param-types)
                       variadic?)))

(defn emit-fac
  []
  (binding [*module* (LLVMModuleCreateWithName "fac")
            *builder* (LLVMCreateBuilder)]
    (let [fn-type (function-type (LLVMInt64Type) [(LLVMInt64Type)])
          fac (doto (LLVMAddFunction *module* "fac" fn-type)
                (LLVMSetFunctionCallConv LLVMCCallConv))
          n (LLVMGetParam fac 0)
          entry (LLVMAppendBasicBlock fac "entry")
          iftrue (LLVMAppendBasicBlock fac "iftrue")
          iffalse (LLVMAppendBasicBlock fac "iffalse")
          end (LLVMAppendBasicBlock fac "end")
          res-iftrue (LLVMConstInt (LLVMInt64Type) 1 false)]
      (LLVMPositionBuilderAtEnd *builder* entry)
      (let [if-cmp (LLVMBuildICmp *builder* LLVMIntEQ n
                                  (LLVMConstInt (LLVMInt64Type) 0 false) "")]
        (LLVMBuildCondBr *builder* if-cmp  iftrue iffalse)
        (LLVMPositionBuilderAtEnd *builder* iftrue)
        (LLVMBuildBr *builder* end)
        (LLVMPositionBuilderAtEnd *builder* iffalse)
        (let [n-minus (LLVMBuildSub *builder* n
                                    (LLVMConstInt (LLVMInt64Type) 1 false)
                                    "")
              call-fac (LLVMBuildCall *builder* fac (values n-minus) 1 "")
              res-iffalse (LLVMBuildMul *builder* n call-fac "")
              err (into-array String [])]
          (LLVMBuildBr *builder* end)
          (LLVMPositionBuilderAtEnd *builder* end)
          (let [res (LLVMBuildPhi *builder* (LLVMInt64Type) "result")
                phi-vals (values res-iftrue res-iffalse)
                phi-blocks (basic-blocks iftrue iffalse)]
            (LLVMAddIncoming res phi-vals phi-blocks 2)
            (LLVMBuildRet *builder* res)
            (if-not (LLVMVerifyModule *module* LLVMPrintMessageAction err)
              (jit/compile-module *module*)
              fac)))))))

(def ptr (first (emit-fac)))

(def native-fac
  (Function/getFunction ptr))




(def dyn-fac
  (.asDynamicFunction (TypedPointer. (Pointer/nativeValue native-fac))
                      Convention$Style/CDecl
                      Long/TYPE (into-array Type [Long/TYPE])))

(def args (to-array [20]))

(.apply ^DynamicFunction dyn-fac args)
