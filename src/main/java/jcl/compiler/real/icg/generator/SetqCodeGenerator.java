package jcl.compiler.real.icg.generator;

import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class SetqCodeGenerator extends SpecialOperatorCodeGenerator<SetqStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	private SetqCodeGenerator() {
		super("setq");
	}

	@Override
	protected void generateSpecialOperator(final SetqStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final List<SetqStruct.SetqPair> setqPairs = input.getSetqPairs();

		final Stack<Environment> bindingStack = generatorState.getBindingStack();
		final Environment currentEnvironment = bindingStack.peek();

		final Integer closureSymbolBindingsStore = methodBuilder.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_GET_CLOSURE_METHOD_DESC,
				false);
		final Integer closureStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		final Label closureNullCheckIfEnd = new Label();
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.CLOSURE_NAME,
				GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME,
				GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		final int initFormStore = methodBuilder.getNextAvailableStore();

		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			final SymbolStruct<?> var = setqPair.getVar();
			CodeGenerators.generateSymbol(var, methodBuilder, packageStore, symbolStore);

			final LispStruct form = setqPair.getForm();
			codeGenerator.generate(form, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			CodeGenerators.generateValuesCheckAndStore(methodBuilder, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(var);
			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);

			if (hasLexicalBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.SYMBOL_STRUCT_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_DESC,
						false);
			} else if (hasDynamicBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.SYMBOL_STRUCT_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_DESC,
						false);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.SYMBOL_STRUCT_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_VALUE_METHOD_NAME,
						GenerationConstants.SYMBOL_STRUCT_SET_VALUE_METHOD_DESC,
						false);
			}

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			final Label closureBindingsNullCheckIfEnd = new Label();
			mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_MAP_NAME,
					GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
					GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureBindingsNullCheckIfEnd);
		}

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
