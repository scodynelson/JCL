package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvCodeGenerator implements CodeGenerator<ProgvStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final ProgvStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct vars = input.getVars();
		final LispStruct vals = input.getVals();
		final PrognStruct forms = input.getForms();
		final ProgvEnvironment progvEnvironment = input.getProgvEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String progvMethodName = "progv_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, progvMethodName, "(Ljcl/functions/Closure;)Ljcl/LispStruct;", null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		final Label finallyBlockStart = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
		mv.visitTryCatchBlock(catchBlockStart, finallyBlockStart, catchBlockStart, null);

		formGenerator.generate(vars, classBuilder);

		final int varsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsStore);

		final Label varsListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/lists/ListStruct");
		mv.visitJumpInsn(Opcodes.IFNE, varsListCheckEnd);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/conditions/exceptions/ProgramErrorException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
		mv.visitLdcInsn("PROGV: Symbols list must be a list. Got: ");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/conditions/exceptions/ProgramErrorException", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(varsListCheckEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/lists/ListStruct");
		final int varsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsListStore);

		mv.visitVarInsn(Opcodes.ALOAD, varsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/lists/ListStruct", "getAsJavaList", "()Ljava/util/List;", false);
		final int varsJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsJavaListStore);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;", true);
		final int varsJavaListIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsJavaListIteratorStore);

		final Label varsListOfSymbolsIteratorLoopStart = new Label();
		final Label varsListOfSymbolsIteratorLoopEnd = new Label();

		mv.visitLabel(varsListOfSymbolsIteratorLoopStart);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);
		mv.visitJumpInsn(Opcodes.IFEQ, varsListOfSymbolsIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
		final int varSymbolCheckStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolCheckStore);

		final Label varSymbolCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolCheckStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/symbols/SymbolStruct");
		mv.visitJumpInsn(Opcodes.IFNE, varSymbolCheckEnd);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/conditions/exceptions/ProgramErrorException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
		mv.visitLdcInsn("PROGV: Elements in symbols list must be symbols. Got: ");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
		mv.visitVarInsn(Opcodes.ALOAD, varSymbolCheckStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/conditions/exceptions/ProgramErrorException", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(varSymbolCheckEnd);

		mv.visitJumpInsn(Opcodes.GOTO, varsListOfSymbolsIteratorLoopStart);

		mv.visitLabel(varsListOfSymbolsIteratorLoopEnd);

		formGenerator.generate(vals, classBuilder);

		final int valsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsStore);

		final Label valsListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/lists/ListStruct");
		mv.visitJumpInsn(Opcodes.IFNE, valsListCheckEnd);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/conditions/exceptions/ProgramErrorException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitTypeInsn(Opcodes.NEW, "java/lang/StringBuilder");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
		mv.visitLdcInsn("PROGV: Values list must be a list. Got: ");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/conditions/exceptions/ProgramErrorException", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(valsListCheckEnd);
		mv.visitVarInsn(Opcodes.ALOAD, valsStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/lists/ListStruct");
		final int valsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsListStore);

		mv.visitVarInsn(Opcodes.ALOAD, valsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/lists/ListStruct", "getAsJavaList", "()Ljava/util/List;", false);
		final int valsJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valsJavaListStore);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "size", "()I", true);
		final int varsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, varsJavaListSizeStore);

		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "size", "()I", true);
		final int valsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, valsJavaListSizeStore);

		// Var Binding Loop

		mv.visitInsn(Opcodes.ICONST_0);
		final int indexValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, indexValueStore);

		final Label varBindingLoopStart = new Label();

		mv.visitLabel(varBindingLoopStart);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, varsJavaListSizeStore);
		mv.visitJumpInsn(Opcodes.IF_ICMPGE, tryBlockStart);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "get", "(I)Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
		final int varSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		final int valStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, valsJavaListSizeStore);

		final Label numberOfValsCheckEnd = new Label();

		mv.visitJumpInsn(Opcodes.IF_ICMPGE, numberOfValsCheckEnd);
		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "get", "(I)Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitLabel(numberOfValsCheckEnd);

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
		final int valuesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitLabel(valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindDynamicValue", "(Ljcl/LispStruct;)V", false);

		mv.visitIincInsn(indexValueStore, 1);
		mv.visitJumpInsn(Opcodes.GOTO, varBindingLoopStart);

		// BODY START
		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(progvEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;", true);
		final int normalUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingIteratorStore);

		final Label normalUnbindingIteratorLoopStart = new Label();
		final Label normalUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(normalUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);
		mv.visitJumpInsn(Opcodes.IFEQ, normalUnbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
		final int normalUnbindingVarStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingVarStore);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingVarStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
		final int normalUnbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, normalUnbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, normalUnbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindDynamicValue", "()V", false);

		mv.visitJumpInsn(Opcodes.GOTO, normalUnbindingIteratorLoopStart);

		mv.visitLabel(normalUnbindingIteratorLoopEnd);

		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		mv.visitLabel(finallyBlockStart);
		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "iterator", "()Ljava/util/Iterator;", true);
		final int exceptionUnbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingIteratorStore);

		final Label exceptionUnbindingIteratorLoopStart = new Label();
		final Label exceptionUnbindingIteratorLoopEnd = new Label();

		mv.visitLabel(exceptionUnbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);
		mv.visitJumpInsn(Opcodes.IFEQ, exceptionUnbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
		final int exceptionUnbindingVarStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingVarStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingVarStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
		final int exceptionUnbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionUnbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionUnbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindDynamicValue", "()V", false);

		mv.visitJumpInsn(Opcodes.GOTO, exceptionUnbindingIteratorLoopStart);

		mv.visitLabel(exceptionUnbindingIteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, progvMethodName, "(Ljcl/functions/Closure;)Ljcl/LispStruct;", false);
	}
}
