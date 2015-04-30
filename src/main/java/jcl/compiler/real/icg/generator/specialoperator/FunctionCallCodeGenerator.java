package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.simple.SymbolCodeGeneratorUtil;
import jcl.compiler.real.struct.specialoperator.FunctionCallStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FunctionCallCodeGenerator implements CodeGenerator<FunctionCallStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final FunctionCallStruct input, final JavaClassBuilder classBuilder) {

		final boolean recursiveCall = input.isRecursiveCall();
		final SymbolStruct<?> functionSymbol = input.getFunctionSymbol();
		final List<LispStruct> arguments = input.getArguments();

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (recursiveCall) {
			tailCallGenerate(methodBuilder, mv, classBuilder, arguments);
		} else {
			final int functionSymbolStore = SymbolCodeGeneratorUtil.generate(functionSymbol, classBuilder);

//			final Environment currentEnvironment = classBuilder.getBindingEnvironment();
//			if (currentEnvironment instanceof FletEnvironment) {
//				final FletEnvironment fletEnvironment = (FletEnvironment) currentEnvironment;
//				fletGenerate(currentClass, mv, classBuilder, fletEnvironment, functionSymbol, arguments, functionSymbolStore);
//			} else {
				nonFletGenerate(methodBuilder, mv, classBuilder, arguments, functionSymbolStore);
//			}
		}
	}

	private void tailCallGenerate(final JavaMethodBuilder methodBuilder, final MethodVisitor mv, final JavaClassBuilder classBuilder,
	                              final List<LispStruct> arguments) {

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "jcl/LispStruct");
		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			formGenerator.generate(argument, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, 0); // TODO: I know that '0' essentially means 'this'. But can we do better by passing the actual Store value around???
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", false);
	}

	private void nonFletGenerate(final JavaMethodBuilder methodBuilder, final MethodVisitor mv, final JavaClassBuilder classBuilder,
	                             final List<LispStruct> arguments, final int functionSymbolStore) {

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getFunction", "()Ljcl/functions/FunctionStruct;", false);
		final int functionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionStore);

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "jcl/LispStruct");
		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			formGenerator.generate(argument, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", false);
	}

//	private void fletGenerate(final ClassDef currentClass, final MethodVisitor mv, final JavaClassBuilder classBuilder,
//	                          final FletEnvironment fletEnvironment, final SymbolStruct<?> functionSymbol,
//	                          final List<LispStruct> arguments, final int functionSymbolStore) {
//
//		final Label tryBlockStart = new Label();
//		final Label tryBlockEnd = new Label();
//		final Label catchBlockStart = new Label();
//		final Label catchBlockEnd = new Label();
//
//		final boolean hasFunctionBinding = fletEnvironment.hasFunctionBinding(functionSymbol);
//		if (hasFunctionBinding) {
//
//			final Map<SymbolStruct<?>, Integer> fletFunctionStoresToBind = classBuilder.getFletFunctionStoresToBind();
//			if (fletFunctionStoresToBind.containsKey(functionSymbol)) {
//				mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
//
//				final Integer closureFunctionBindingsStore = currentClass.getNextAvailableStore();
//
//				mv.visitVarInsn(Opcodes.ALOAD, 0);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "getClosure", "()Ljcl/functions/Closure;", false);
//				final Integer closureStore = currentClass.getNextAvailableStore();
//				mv.visitVarInsn(Opcodes.ASTORE, closureStore);
//
//				mv.visitInsn(Opcodes.ACONST_NULL);
//				mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, closureStore);
//				final Label closureNullCheckIfEnd = new Label();
//				mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);
//
//				mv.visitVarInsn(Opcodes.ALOAD, closureStore);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/Closure", "getFunctionBindings", "()Ljava/util/Map;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);
//
//				mv.visitLabel(closureNullCheckIfEnd);
//
//				final Integer initFormStore = fletFunctionStoresToBind.get(functionSymbol);
//
//				mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindFunction", "(Ljcl/functions/FunctionStruct;)V", false);
//
//				mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
//				final Label closureBindingsNullCheckIfEnd = new Label();
//				mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);
//
//				mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
//				mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
//				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", true);
//				mv.visitInsn(Opcodes.POP);
//
//				mv.visitLabel(closureBindingsNullCheckIfEnd);
//
//				mv.visitLabel(tryBlockStart);
//			}
//		}
//
//		nonFletGenerate(currentClass, mv, classBuilder, arguments, functionSymbolStore);
//
//		if (hasFunctionBinding) {
//
//			final Map<SymbolStruct<?>, Integer> fletFunctionStoresToBind = classBuilder.getFletFunctionStoresToBind();
//			if (fletFunctionStoresToBind.containsKey(functionSymbol)) {
//				final int resultStore = currentClass.getNextAvailableStore();
//				mv.visitVarInsn(Opcodes.ASTORE, resultStore);
//
//				mv.visitLabel(tryBlockEnd);
//				generateFinallyCode(mv, functionSymbolStore);
//				mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);
//
//				mv.visitLabel(catchBlockStart);
//				final int exceptionStore = currentClass.getNextAvailableStore();
//				mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);
//
//				generateFinallyCode(mv, functionSymbolStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
//				mv.visitInsn(Opcodes.ATHROW);
//
//				mv.visitLabel(catchBlockEnd);
//				mv.visitVarInsn(Opcodes.ALOAD, resultStore);
//			}
//		}
//	}
//
//	private void generateFinallyCode(final MethodVisitor mv, final Integer functionSymbolStore) {
//		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
//		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindFunction", "()V", false);
//	}
}
