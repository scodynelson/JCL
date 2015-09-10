package jcl.compiler.real.icg.generator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.FletStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class FletCodeGenerator extends SpecialOperatorCodeGenerator<FletStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private FletCodeGenerator() {
		super("flet_");
	}

	@Override
	protected void generateSpecialOperator(final FletStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final List<FletStruct.FletVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final FletEnvironment fletEnvironment = input.getLexicalEnvironment();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final int closureFunctionBindingsStore = methodBuilder.getNextAvailableStore();

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		final Label closureNullCheckIfEnd = new Label();
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.CLOSURE_NAME,
				GenerationConstants.CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME,
				GenerationConstants.CLOSURE_GET_FUNCTION_BINDINGS_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final int packageStore = methodBuilder.getNextAvailableStore();

		final Map<Integer, Integer> functionStoresToBind = new HashMap<>();

		for (final FletStruct.FletVar var : vars) {
			final SymbolStruct<?> functionSymbolVar = var.getVar();
			// NOTE: we have to get a new 'functionSymbolStore' for each var so we can properly unbind the expansions later
			final int functionSymbolStore = methodBuilder.getNextAvailableStore();
			SymbolCodeGeneratorUtil.generate(functionSymbolVar, generatorState, packageStore, functionSymbolStore);

			final CompilerFunctionStruct initForm = var.getInitForm();
			codeGenerator.generate(initForm, generatorState);
			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			functionStoresToBind.put(functionSymbolStore, initFormStore);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : functionStoresToBind.entrySet()) {
			final Integer functionSymbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_DESC,
					false);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			final Label closureBindingsNullCheckIfEnd = new Label();
			mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_MAP_NAME,
					GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
					GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureBindingsNullCheckIfEnd);
		}

		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = generatorState.getBindingStack();

		bindingStack.push(fletEnvironment);
		prognCodeGenerator.generate(forms, generatorState);
		bindingStack.pop();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		final Set<Integer> varSymbolStores = functionStoresToBind.keySet();

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, varSymbolStores);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, varSymbolStores);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	private static void generateFinallyCode(final MethodVisitor mv, final Set<Integer> varSymbolStores) {
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC,
					false);
		}
	}
}
