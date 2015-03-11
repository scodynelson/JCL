package jcl.compiler.real.icg.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.PositionAllocation;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class LetCodeGenerator implements CodeGenerator<ListStruct> {

	private static final class SymbolBindingLabel {

		final Label endLabel;
		final Label finallyLabel;
		final Label handlerLabel;
		final SymbolStruct<?> dynamicSymbol;

		private SymbolBindingLabel(final Label endLabel, final Label finallyLabel, final Label handlerLabel, final SymbolStruct<?> dynamicSymbol) {
			this.endLabel = endLabel;
			this.finallyLabel = finallyLabel;
			this.handlerLabel = handlerLabel;
			this.dynamicSymbol = dynamicSymbol;
		}
	}

	public static final LetCodeGenerator INSTANCE = new LetCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// ((%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
		final Stack<SymbolBindingLabel> bindingLabels = new Stack<>();

		// are we building a closure here?
		//----->
		classBuilder.setBindingEnvironment(classBuilder.getBindingStack().push((Environment) input.getFirst()));
		final Closure closureSetBody = classBuilder.getBindingEnvironment().getClosure();
//        int numParams = closureSetBody.size() - 1;

		try {
			// (%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...))
			// Handle all of the binding information
			//----->
			final Environment bindings = classBuilder.getBindingEnvironment();
			// ((:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
			// Now get just the bindings list and drop the :bindings
			final List<EnvironmentParameterBinding> lexicalBindingList = classBuilder.getBindingEnvironment().getLexicalBindings();
			// ((sym1 :allocation ... :binding ... :scope ... :type ... :init-form ...)
			//  (sym2 :allocation ... :binding ... :scope ... :type ... :init-form ...)...)
			// Now to loop thru the bindings, gen code for the init forms and store them in the
			// proper slots. Note that init forms are evaluated in the enclosing environment
			final Environment tmpEnv = classBuilder.getBindingEnvironment();
			// any init forms get evaluated in the parent binding
			classBuilder.setBindingEnvironment(classBuilder.getBindingEnvironment().getParent());
			// now, run the bindings
			for (final EnvironmentParameterBinding binding : lexicalBindingList) {
				final SymbolStruct<?> sym = binding.getSymbolStruct();
				// (:allocation ... :binding ... :scope ... :type ... :init-form ...)
				// get the variable's init form
				final LispStruct initForm = binding.getInitForm();
				// is this a local or dynamic variable?
				//** this is the place where the ICG has to choose to allocate a variable
				//** in a local or it's a binding of a special variable
				// Now get the allocation value
				final PositionAllocation alloc = binding.getAllocation();
				final int slot = alloc.getPosition();
				// hand the init form to icgMainLoop...
				// the generated code leaves its value on the stack
				codeGenerator.icgMainLoop(initForm, classBuilder);
				// store the value in the proper local slot
				classBuilder.getEmitter().emitAstore(slot);
			}

			final List<EnvironmentBinding<?>> dynamicBindingList = classBuilder.getBindingEnvironment().getDynamicBindings();
			for (final EnvironmentBinding<?> binding : dynamicBindingList) {
				final SymbolStruct<?> sym = binding.getSymbolStruct();
				// (:allocation ... :binding ... :scope ... :type ... :init-form ...)
				// get the variable's init form
				LispStruct initForm = null;
				if (binding instanceof EnvironmentParameterBinding) {
					initForm = ((EnvironmentParameterBinding) binding).getInitForm();
				} else if (binding instanceof EnvironmentEnvironmentBinding) {
					final Environment bindingEnvironment = ((EnvironmentEnvironmentBinding) binding).getEnvironment();
					initForm = ((EnvironmentParameterBinding) bindingEnvironment.getDynamicBinding(sym).get()).getInitForm();
				}

				// is this a local or dynamic variable?
				//** this is the place where the ICG has to choose to allocate a variable
				//** in a local or it's a binding of a special variable
				// handle binding a dynamic variable
				// 0. create an end and a handler Label, add them to a stack, create a start Label
				final Label startLabel = new Label();
				final Label endLabel = new Label();
				final Label finallyLabel = new Label();
				final Label handlerLabel = new Label();
				final SymbolBindingLabel labelSym = new SymbolBindingLabel(endLabel, finallyLabel, handlerLabel, sym);
				// 1. emit the tryFinally node with these labels
				classBuilder.getEmitter().visitTryCatchBlock(startLabel, endLabel, handlerLabel, null);
				// 2. emit the binding call
				codeGenerator.genCodeSpecialVariable(sym, classBuilder);
				classBuilder.getEmitter().emitCheckcast("lisp/system/SymbolImpl");
				// 3. emit the eval of the init form
				// hand the init form to icgMainLoop...
				// the generated code leaves its value on the stack
				codeGenerator.icgMainLoop(initForm, classBuilder);
				classBuilder.getEmitter().emitInvokevirtual("lisp/system/SymbolImpl", "bind", "(Ljava/lang/Object;)", "V", false);
				// 4. set handler start label
				classBuilder.getEmitter().visitMethodLabel(startLabel);
				// 5. push end/handler label and the symbol on a stack
				bindingLabels.push(labelSym);
			}
			classBuilder.setBindingEnvironment(tmpEnv);

			// we may have a closure to handle as well
			IntermediateCodeGenerator.doClosureSetup(classBuilder.getBindingEnvironment(), classBuilder);
			codeGenerator.doFreeVariableSetup(classBuilder);

			// all args are in the proper local slots, so do the body of the let
			final List<LispStruct> copyListJavaList = input.getAsJavaList();
			final ListStruct copyList = ListStruct.buildProperList(copyListJavaList);
			ListStruct funcallList = copyList.getRest();

			while (!NullStruct.INSTANCE.equals(funcallList)) {
				final Object firstElt = funcallList.getFirst();
				if ((firstElt instanceof ListStruct) && ((ListStruct) firstElt).getFirst().equals(SpecialOperator.DECLARE)) {
					funcallList = funcallList.getRest();
				} else {
					codeGenerator.icgMainLoop(funcallList.getFirst(), classBuilder);
					funcallList = funcallList.getRest();
					if (!NullStruct.INSTANCE.equals(funcallList)) {
						classBuilder.getEmitter().emitPop();
					}
				}
			}

			// Now we construct the set of unbinds that constitutes the finally blocks
			// -> pop off labels on stack...
			while (!bindingLabels.empty()) {
				final Label outLabel = new Label();
				// 1. emit the end/handler label
				final SymbolBindingLabel labelSym = bindingLabels.pop();
				classBuilder.getEmitter().visitMethodLabel(labelSym.endLabel); // end of the try block
				// now call the finally block
				codeGenerator.genCodeSpecialVariable(labelSym.dynamicSymbol, classBuilder);
				classBuilder.getEmitter().emitCheckcast("lisp/system/SymbolImpl");
				classBuilder.getEmitter().emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				classBuilder.getEmitter().emitPop(); // would mask the real return
				// now jump to the end of this block
				classBuilder.getEmitter().emitGoto(outLabel);

				// now for the handler part
				classBuilder.getEmitter().visitMethodLabel(labelSym.handlerLabel);
				// I have no idea why adding this DUP works, but it does...
				classBuilder.getEmitter().emitDup();
				classBuilder.getEmitter().emitAstore(1); // save the exception
				codeGenerator.genCodeSpecialVariable(labelSym.dynamicSymbol, classBuilder);
				classBuilder.getEmitter().emitCheckcast("lisp/system/SymbolImpl");
				classBuilder.getEmitter().emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				classBuilder.getEmitter().emitPop(); // would mask the real return
				classBuilder.getEmitter().emitAload(1); // reload the exception
				classBuilder.getEmitter().emitAthrow(); // re-throw it

				// 2. emit unbind (finally clause) - it gets there either way
				classBuilder.getEmitter().visitMethodLabel(labelSym.finallyLabel); // start of the finally block
				// -- however it gets into the sequence of unbinds, it just runs them in
				// -- reverse order of binding
				classBuilder.getEmitter().visitMethodLabel(outLabel);
			}
		} finally {
			classBuilder.getBindingStack().pop();
			classBuilder.setBindingEnvironment(classBuilder.getBindingStack().peek());
		}
	}
}
