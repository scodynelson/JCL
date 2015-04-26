package jcl.compiler.real.icg.generator.specialoperator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
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
public class FletCodeGenerator implements CodeGenerator<FletStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final FletStruct input, final JavaClassBuilder classBuilder) {

		final List<FletStruct.FletVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final FletEnvironment fletEnvironment = input.getLexicalEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final Integer closureFunctionBindingsStore = currentClass.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "getClosure", "()Ljcl/functions/Closure;", false);
		final Integer closureStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		final Label closureNullCheckIfEnd = new Label();
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/Closure", "getFunctionBindings", "()Ljava/util/Map;", false);
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final int packageStore = currentClass.getNextAvailableStore();

		final Map<Integer, Integer> functionStoresToBind = new HashMap<>();

		for (final FletStruct.FletVar var : vars) {
			final SymbolStruct<?> functionSymbolVar = var.getVar();
			final CompilerFunctionStruct initForm = var.getInitForm();

			final String packageName = functionSymbolVar.getSymbolPackage().getName();
			final String symbolName = functionSymbolVar.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			// NOTE: we have to get a new 'functionSymbolStore' for each var so we can properly unbind the expansions later
			final int functionSymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, functionSymbolStore);

			formGenerator.generate(initForm, classBuilder);
			final int initFormStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			functionStoresToBind.put(functionSymbolStore, initFormStore);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : functionStoresToBind.entrySet()) {
			final Integer functionSymbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindFunction", "(Ljcl/functions/FunctionStruct;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			final Label closureBindingsNullCheckIfEnd = new Label();
			mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureBindingsNullCheckIfEnd);
		}

		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(fletEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		final Set<Integer> varSymbolStores = functionStoresToBind.keySet();

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, varSymbolStores);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, varSymbolStores);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
	}

	private void generateFinallyCode(final MethodVisitor mv, final Set<Integer> varSymbolStores) {
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindFunction", "()V", false);
		}
	}
}
