package jcl.compiler.real.icg.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.allocation.PositionAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.element.Element;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;

import java.util.List;
import java.util.Stack;

public class LetCodeGenerator implements CodeGenerator<ListStruct> {

	private static class SymbolBindingLabel {

		Label endLabel = null;
		Label finallyLabel = null;
		Label handlerLabel = null;
		SymbolStruct<?> dynamicSymbol = null;

		private SymbolBindingLabel(final Label endLabel, final Label finallyLabel, final Label handlerLabel, final SymbolStruct<?> dynamicSymbol) {
			this.endLabel = endLabel;
			this.finallyLabel = finallyLabel;
			this.handlerLabel = handlerLabel;
			this.dynamicSymbol = dynamicSymbol;
		}
	}

	public static final LetCodeGenerator INSTANCE = new LetCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		// ((%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
		final Stack<SymbolBindingLabel> bindingLabels = new Stack<>();

		// are we building a closure here?
		//----->
		codeGenerator.bindingEnvironment = codeGenerator.bindingStack.push((LexicalEnvironment) input.getFirst());
		final Closure closureSetBody = codeGenerator.bindingEnvironment.getClosure();
//        int numParams = closureSetBody.size() - 1;

		try {
			// (%let... (:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...))
			// Handle all of the binding information
			//----->
			final LexicalEnvironment bindings = codeGenerator.bindingEnvironment;
			// ((:parent ...) (:bindings ...) (:symbol-table ...) (:closure ...)))
			// Now get just the bindings list and drop the :bindings
			final List<EnvironmentBinding> bindingList = codeGenerator.bindingEnvironment.getBindings();
			// ((sym1 :allocation ... :binding ... :scope ... :type ... :init-form ...)
			//  (sym2 :allocation ... :binding ... :scope ... :type ... :init-form ...)...)
			// Now to loop thru the bindings, gen code for the init forms and store them in the
			// proper slots. Note that init forms are evaluated in the enclosing environment
			final LexicalEnvironment tmpEnv = codeGenerator.bindingEnvironment;
			// any init forms get evaluated in the parent binding
			codeGenerator.bindingEnvironment = codeGenerator.bindingEnvironment.getParent();
			// now, run the bindings
			for (final Binding<?> binding : bindingList) {
				final SymbolStruct<?> sym = binding.getSymbolStruct();
				// (:allocation ... :binding ... :scope ... :type ... :init-form ...)
				// get the variable's init form
				final Element initForm = ((EnvironmentBinding) binding).getInitForm();
				// is this a local or dynamic variable?
				final Scope scope = binding.getScope();
				//** this is the place where the ICG has to choose to allocate a variable
				//** in a local or it's a binding of a special variable
				// now, which is it: :local or :dynamic
				if (scope == Scope.DYNAMIC) {
					// handle binding a dynamic variable
					// 0. create an end and a handler Label, add them to a stack, create a start Label
					final Label startLabel = new Label();
					final Label endLabel = new Label();
					final Label finallyLabel = new Label();
					final Label handlerLabel = new Label();
					final SymbolBindingLabel labelSym = new SymbolBindingLabel(endLabel, finallyLabel, handlerLabel, sym);
					// 1. emit the tryFinally node with these labels
					codeGenerator.emitter.visitTryCatchBlock(startLabel, endLabel, handlerLabel, null);
					// 2. emit the binding call
					codeGenerator.genCodeSpecialVariable(sym);
					codeGenerator.emitter.emitCheckcast("lisp/system/SymbolImpl");
					// 3. emit the eval of the init form
					// hand the init form to icgMainLoop...
					// the generated code leaves its value on the stack
					codeGenerator.icgMainLoop(initForm);
					codeGenerator.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "bind", "(Ljava/lang/Object;)", "V", false);
					// 4. set handler start label
					codeGenerator.emitter.visitMethodLabel(startLabel);
					// 5. push end/handler label and the symbol on a stack
					bindingLabels.push(labelSym);
				} else {
					// Now get the allocation value
					final PositionAllocation alloc = (PositionAllocation) binding.getAllocation();
					final int slot = alloc.getPosition();
					// hand the init form to icgMainLoop...
					// the generated code leaves its value on the stack
					codeGenerator.icgMainLoop(initForm);
					// store the value in the proper local slot
					codeGenerator.emitter.emitAstore(slot);
				}
			}
			codeGenerator.bindingEnvironment = tmpEnv;

			// we may have a closure to handle as well
			codeGenerator.doClosureSetup(codeGenerator.bindingEnvironment);
			codeGenerator.doFreeVariableSetup();

			// all args are in the proper local slots, so do the body of the let
			final List<LispStruct> copyListJavaList = input.getAsJavaList();
			final ListStruct copyList = ListStruct.buildProperList(copyListJavaList);
			ListStruct funcallList = copyList.getRest();

			while (!NullStruct.INSTANCE.equals(funcallList)) {
				final Object firstElt = funcallList.getFirst();
				if ((firstElt instanceof ListStruct) && ((ListStruct) firstElt).getFirst().equals(SpecialOperator.DECLARE)) {
					funcallList = funcallList.getRest();
				} else {
					codeGenerator.icgMainLoop(funcallList.getFirst());
					funcallList = funcallList.getRest();
					if (!NullStruct.INSTANCE.equals(funcallList)) {
						codeGenerator.emitter.emitPop();
					}
				}
			}

			// Now we construct the set of unbinds that constitutes the finally blocks
			// -> pop off labels on stack...
			while (!bindingLabels.empty()) {
				final Label outLabel = new Label();
				// 1. emit the end/handler label
				final SymbolBindingLabel labelSym = bindingLabels.pop();
				codeGenerator.emitter.visitMethodLabel(labelSym.endLabel); // end of the try block
				// now call the finally block
				codeGenerator.genCodeSpecialVariable(labelSym.dynamicSymbol);
				codeGenerator.emitter.emitCheckcast("lisp/system/SymbolImpl");
				codeGenerator.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				codeGenerator.emitter.emitPop(); // would mask the real return
				// now jump to the end of this block
				codeGenerator.emitter.emitGoto(outLabel);

				// now for the handler part
				codeGenerator.emitter.visitMethodLabel(labelSym.handlerLabel);
				// I have no idea why adding this DUP works, but it does...
				codeGenerator.emitter.emitDup();
				codeGenerator.emitter.emitAstore(1); // save the exception
				codeGenerator.genCodeSpecialVariable(labelSym.dynamicSymbol);
				codeGenerator.emitter.emitCheckcast("lisp/system/SymbolImpl");
				codeGenerator.emitter.emitInvokevirtual("lisp/system/SymbolImpl", "unbind", "()", "Ljava/lang/Object;", false);
				codeGenerator.emitter.emitPop(); // would mask the real return
				codeGenerator.emitter.emitAload(1); // reload the exception
				codeGenerator.emitter.emitAthrow(); // re-throw it

				// 2. emit unbind (finally clause) - it gets there either way
				codeGenerator.emitter.visitMethodLabel(labelSym.finallyLabel); // start of the finally block
				// -- however it gets into the sequence of unbinds, it just runs them in
				// -- reverse order of binding
				codeGenerator.emitter.visitMethodLabel(outLabel);
			}
		} finally {
			codeGenerator.bindingStack.pop();
			codeGenerator.bindingEnvironment = codeGenerator.bindingStack.peek();
		}
	}
}
