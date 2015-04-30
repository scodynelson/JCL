package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchCodeGenerator implements CodeGenerator<CatchStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final CatchStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct catchTag = input.getCatchTag();
		final PrognStruct forms = input.getForms();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String catchMethodName = "catch_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, catchMethodName, "()Ljcl/LispStruct;", null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");

		formGenerator.generate(catchTag, classBuilder);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		mv.visitLabel(tryBlockStart);
		prognCodeGenerator.generate(forms, classBuilder);
		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int throwExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getCatchTag", "()Ljcl/LispStruct;", false);
		final int throwExceptionCatchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionCatchTagStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionCatchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getResultForm", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, 0);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, catchMethodName, "()Ljcl/LispStruct;", false);
	}
}
