package jcl.compiler.real.icg.generator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class SymbolFunctionCallCodeGenerator implements CodeGenerator<SymbolFunctionCallStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Override
	public void generate(final SymbolFunctionCallStruct input, final GeneratorState generatorState) {

		final boolean recursiveCall = input.isRecursiveCall();
		final SymbolStruct<?> functionSymbol = input.getFunctionSymbol();
		final List<LispStruct> arguments = input.getArguments();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (recursiveCall) {
			tailCallGenerate(methodBuilder, mv, generatorState, arguments);
		} else {
			final int functionPackageStore = methodBuilder.getNextAvailableStore();
			final int functionSymbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(functionSymbol, methodBuilder, functionPackageStore, functionSymbolStore);

			nonTailCallGenerate(methodBuilder, mv, generatorState, arguments, functionSymbolStore);
		}
	}

	private void tailCallGenerate(final JavaMethodBuilder methodBuilder, final MethodVisitor mv, final GeneratorState classBuilder,
	                              final List<LispStruct> arguments) {

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			codeGenerator.generate(argument, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, 0); // this.apply(...)
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
				false);
	}

	private void nonTailCallGenerate(final JavaMethodBuilder methodBuilder, final MethodVisitor mv, final GeneratorState classBuilder,
	                                 final List<LispStruct> arguments, final int functionSymbolStore) {

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_DESC,
				false);
		final int functionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionStore);

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			codeGenerator.generate(argument, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
				false);
	}
}
