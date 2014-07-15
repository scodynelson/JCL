package jcl.compiler.real.sa.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.MacroFunctionBinding;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.FunctionAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class FunctionMarkerAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionMarkerAnalyzer INSTANCE = new FunctionMarkerAnalyzer();

	@Override
	public LispStruct analyze(ListStruct input) {
		ListStruct cpy = input;
		// check to see if there's a function binding in existence
		if (input.getFirst() instanceof SymbolStruct) {
			SymbolStruct fnName = (SymbolStruct) input.getFirst();
			//TODO make this active only during compiling, not during eval during compile-file
			// handle messaging the argument list according to the lambda list
			// if the car of the function is a special marker, drop the marker
			if (fnName.equals(SpecialOperator.FUNCTION_MARKER)) {
				// drop the marker
				input = input.getRest();
				cpy = input;
			} else { // not already munged
				final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(SemanticAnalyzer.environmentStack.peek(), fnName, false);
				if (!fnBinding.equals(Environment.NULL)) {
					// use assoc to get bindings
					final MacroFunctionBinding fooBinding = (MacroFunctionBinding) fnBinding.getBinding(fnName);
					// see if this is a LABELS or FLET. That changes fnName
					fnName = fooBinding.getName();
					input.setElement(1, fnName);
					SemanticAnalyzer.currentLispName.push(fnName);
					return analyze(input);
				}
				final ListStruct args = input.getRest();
				final FunctionStruct fnApplying = fnName.getFunction(); // (fnApplying ....)
				if ((fnApplying == null) && SemanticAnalyzer.undefinedFunctions.contains(fnName)) {
					// add this as a possible undefined function
					SemanticAnalyzer.undefinedFunctions.add(fnName);
				}
				// NOW - whew...
				// Get the application munger for this function
				// If there is a function, get the munger from the function.
				// If not, use the default one that comes with the FunctionBaseClass
				FunctionStruct munger = SemanticAnalyzer.LAMBDA_ARGLIST_MUNGER;
				try {
					if ((fnApplying == null) && SemanticAnalyzer.currentLispName.peek().equals(fnName)) {
						// the function is not known at this point but
						// this is a recursive call and there will be a munger already created
						final ListStruct inProcessMunger = AssocFunction.funcall(fnName, SemanticAnalyzer.currentArgMunger);
						if (!inProcessMunger.equals(NullStruct.INSTANCE)) {
							munger = (FunctionStruct) ((ConsStruct) inProcessMunger).getCdr();
						}
						SemanticAnalyzer.currentLispName.pop();
					} else if (fnApplying != null) {
						// here we know that the function was compiled and has a munger
						munger = (FunctionStruct) fnApplying.getClass().getField(FunctionAnalyzer.LAMBDA_ARGLIST_MUNGER_STRING.getAsJavaString()).get(null);
					} // else gets the default munger
					assert munger != null;
				} catch (final Exception ex) {
					throw new RuntimeException(
							"Unable to get munging function from fn " + fnApplying, ex);
				}
				input = (ListStruct) munger.apply(input.getRest(), input.getFirst());
				return (ListStruct) SemanticAnalyzer.saMainLoop(input);
			}
		}
		input = input.getRest();
		while (!input.equals(NullStruct.INSTANCE)) {
			input.setElement(1, SemanticAnalyzer.saMainLoop(input.getFirst()));
			input = input.getRest();
		}
		// Now it's possible that this is a recursive call to the current function
		// if so, it has to tagged as such to the icg

		if (cpy.getFirst().equals(SemanticAnalyzer.currentLispName.peek())) {
			cpy = new ConsStruct(SpecialOperator.TAIL_RECURSION, cpy);
		}
		return cpy;
	}

}
