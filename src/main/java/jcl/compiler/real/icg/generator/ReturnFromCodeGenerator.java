package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class ReturnFromCodeGenerator implements CodeGenerator<ReturnFromStruct> {

	@Autowired
	private FormGenerator formGenerator;

	private static final String RETURN_FROM_METHOD_NAME_PREFIX = "returnFrom_";

	private static final String RETURN_FROM_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String RETURN_FROM_EXCEPTION_INIT_DESC = GeneratorUtils.getConstructorDescription(ReturnFromException.class, SymbolStruct.class, LispStruct.class);

	@Override
	public void generate(final ReturnFromStruct input, final GeneratorState generatorState) {

		final SymbolStruct<?> name = input.getName();
		final LispStruct result = input.getResult();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String returnFromMethodName = RETURN_FROM_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, returnFromMethodName, RETURN_FROM_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(name, generatorState, namePackageStore, nameSymbolStore);

		formGenerator.generate(result, generatorState);
		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				RETURN_FROM_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, returnFromMethodName, RETURN_FROM_METHOD_DESC, false);
	}
}
