package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.simple.SymbolCodeGeneratorUtil;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockCodeGenerator implements CodeGenerator<BlockStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private static final String BLOCK_METHOD_NAME_PREFIX = "block_";

	private static final String BLOCK_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	@Override
	public void generate(final BlockStruct input, final JavaClassBuilder classBuilder) {

		final SymbolStruct<?> name = input.getName();
		final PrognStruct forms = input.getForms();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String blockMethodName = BLOCK_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, blockMethodName, BLOCK_METHOD_DESC, null, null);

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
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(name, classBuilder, namePackageStore, nameSymbolStore);

		mv.visitLabel(tryBlockStart);
		prognCodeGenerator.generate(forms, classBuilder);
		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int returnFromExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_DESC,
				false);
		final int returnFromExceptionNameStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionNameStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionNameStore);
		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
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
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, blockMethodName, BLOCK_METHOD_DESC, false);
	}
}
