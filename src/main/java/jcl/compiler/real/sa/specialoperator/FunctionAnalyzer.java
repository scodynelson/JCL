package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.XCopyTreeFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.compiler.real.sa.specialoperator.compiler.StaticFieldAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.IdentityHashMap;

public class FunctionAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionAnalyzer INSTANCE = new FunctionAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		if (input.size() != 2) {
			throw new RuntimeException("Wrong number of arguments to special operator Function: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof SymbolStruct) {
			final SymbolStruct<?> functionSymbol = (SymbolStruct) second;
			final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(SemanticAnalyzer.environmentStack.peek(), functionSymbol, false);

			if (fnBinding.equals(Environment.NULL)) {
				SymbolStructAnalyzer.INSTANCE.analyze(functionSymbol);
				return input;
			} else {
				final FunctionBinding functionBinding = (FunctionBinding) fnBinding.getBinding(functionSymbol);
				final SymbolStruct<?> functionBindingName = functionBinding.getName();

				final LispStruct first = input.getFirst();
				return new ConsStruct(first, functionBindingName);
			}
		} else if (second instanceof ListStruct) {
			final ListStruct functionList = (ListStruct) second;

			final LispStruct functionListFirst = functionList.getFirst();
			if (functionListFirst.equals(SpecialOperator.LAMBDA) || functionListFirst.equals(SpecialOperator.MACRO_LAMBDA)) {
				return saLambdaAux(functionList);
			}

			if (functionListFirst.equals(GlobalPackageStruct.COMMON_LISP.findSymbol("SETF").getSymbolStruct())) {
				return input; // no changes at this level
			}

			throw new RuntimeException("Improperly Formed Function: if the first argument is a ListStruct, it must be a LAMBDA");
		}

		throw new RuntimeException("Improperly Formed Function: the arguments must be either a ListStruct or SymbolStruct");
	}

	private static ListStruct saLambdaAux(ListStruct list) {
		if (list.size() < 2) {
			throw new RuntimeException("Incorrectly formed lambda expression, size: " + list.size());
		}

		//might also need here prev. bindings position num, or even a stack?
		final int tempPosition = SemanticAnalyzer.bindingsPosition;
		final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();

		// keep track of the current environment as it is "now"
		final Environment newEnvironment;
		// make a new environment and set it to "current"
		final SymbolStruct operator = (SymbolStruct) list.getFirst();
		if (operator.equals(SpecialOperator.MACRO_LAMBDA)) {
			newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACRO);
		} else {
			newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LAMBDA);
		}
		// set the current environment's parent to what was the environment
		SemanticAnalyzer.environmentStack.push(EnvironmentAccessor.createParent(newEnvironment, parentEnvironment));
		try {
			SemanticAnalyzer.dupSetStack.push(SemanticAnalyzer.dupSet);
			SemanticAnalyzer.dupSet = new IdentityHashMap<>();

			// list => (%lambda (lambda-list) (declare ...) ?"...doc..." body...)
			final SymbolStruct lambdaSym = (SymbolStruct) list.getFirst(); // hang on to %lambda or %macro

			list = list.getRest(); // step over lambda
			// list -> ((lambda-list) ?(declare ...) ?"...doc..." body...)
			// get the parameter list
			ListStruct params = (ListStruct) list.getFirst();
			// params -> (lambda-list)
			list = list.getRest(); // step over the parameter list
			// list -> (?(declare ...)  ?"...doc..."body...)
			// handle any declarations and doc string that might be present
			// returns an updated list with at least one DECLARATION
			// doc string, if present, is now in a declaration
			list = SemanticAnalyzer.saDeclarations(list);
			// list -> ((declare ...) body...)
			final ListStruct decls = (ListStruct) list.getFirst();
			// decls -> (declare ...)

			// Now, see if there is already a parsed lambda list
			// this would happen when the lambda was generated from a defun
			ListStruct theParsedLambdaList = NullStruct.INSTANCE;
			ListStruct auxValues = null;
			if (!params.equals(NullStruct.INSTANCE)) {
				if (theParsedLambdaList.equals(NullStruct.INSTANCE)) {
					final PackageSymbolStruct pOLL = GlobalPackageStruct.COMPILER.findSymbol("PARSE-ORDINARY-LAMBDA-LIST");
					FunctionStruct parseOrdinaryLambdaListFn = null;
					if (pOLL.getPackageSymbolType() != null) {
						parseOrdinaryLambdaListFn = pOLL.getSymbolStruct().getFunction();
					}
					if ((parseOrdinaryLambdaListFn != null) && // the Lisp code is loaded
							lambdaSym.equals(SpecialOperator.LAMBDA)) { // it's a lambda not a macro
						final ListStruct parsedLambdaListParts = (ListStruct) parseOrdinaryLambdaListFn.apply(params);
						theParsedLambdaList = (ListStruct) parsedLambdaListParts.getFirst();
						auxValues = (ListStruct) parsedLambdaListParts.getElement(1);
						// Now, use the Compiler function provide-init-form-usage to create code
						// that follows the declaration section. It is a (possibly nil) set of conditional
						// assignments that fill in the default forms for missing arguments

						// Add the lambda bindings to the current environment
						final ConsStruct bindingList = (ConsStruct) EnvironmentAccessor.getBindingSet(newEnvironment);
						// Now I have to remove any FAKE rest forms. They can't be in the binding set
						bindingList.setCdr(SemanticAnalyzer.removeFakeRestEntry(theParsedLambdaList));
					}
				}
			}
			// temporary hack to handle tail-called functions here
			SemanticAnalyzer.currentParsedLambdaList.push(theParsedLambdaList);

			// classname is a declaration in the declare form
			// if there isn't one, it makes one up
			final SymbolStruct classname = SemanticAnalyzer.saGetClassName(decls);

			// now reconstitute the full lambda form
			list = new ConsStruct(params, list);
			list = new ConsStruct(lambdaSym, list);
			// list => (%lambda (lambda-list) (declare ...) body...)
			final ListStruct cpy = list;

			// step over the lambda SymbolStruct to work on the params etc
			list = list.getRest();

			//TODO This section will be replaced with the Lisp parser
			// the current min for binding is 1 since it's a lambda
			SemanticAnalyzer.bindingsPosition = 0;
			// handled setting up the bindings differently between using the old way and the
			if (!params.equals(NullStruct.INSTANCE)) {
				if (!theParsedLambdaList.equals(NullStruct.INSTANCE) && lambdaSym.equals(SpecialOperator.LAMBDA)) {
					// NOW - the first thing to do is make the arglist analyzer function for this lambda
					// NOTE: this is all skipped when the lambda has a declaration of %NO-GENERATE-ANALYZER
					final PackageSymbolStruct genAnalyzerSym = GlobalPackageStruct.COMPILER.findSymbol("GENERATE-ARGLIST-ANALYZER");
					final FunctionStruct genAnalyzerFn = genAnalyzerSym.getSymbolStruct().getFunction();
					// don't an analyzer for the analyzer!
					if ((genAnalyzerFn != null)
							&& SemanticAnalyzer.findDeclaration(Declaration.NO_GENERATE_ANALYZER, decls.getRest()).equals(NullStruct.INSTANCE)) {
						final ListStruct analyzerFn = (ListStruct) genAnalyzerFn.apply(theParsedLambdaList);
						//*** Under some certain circumstances, the function has to be compiled for use in the rest
						//*** of the function definition. The most common reason is the function is recursive
						//*** and the arglist must be munged before the function is compiled
						// If the function that's being compiled is explicitly named,
						// then we compile the munger and put it into an association list keyed by function name
						if (SemanticAnalyzer.currentLispName.peek() != null) {
							final ListStruct copyFn = (ListStruct) XCopyTreeFunction.FUNCTION.funcall(analyzerFn);
							final FunctionStruct innerFn = (FunctionStruct) CompileFunction.FUNCTION.funcall(copyFn);
							final FunctionStruct munger = (FunctionStruct) innerFn.apply();
							SemanticAnalyzer.currentArgMunger = new ConsStruct(new ConsStruct(SemanticAnalyzer.currentLispName.peek(), munger), SemanticAnalyzer.currentArgMunger);
						}

						// This has created an unevaluated lambda form. We now
						// have to stick it into a static field in the current lambda
						StaticFieldAnalyzer.INSTANCE.analyze(ListStruct.buildProperList(NullStruct.INSTANCE, analyzerFn, SemanticAnalyzer.LAMBDA_ARGLIST_MUNGER_STRING));
						// this static field holds lambdas that provide init values. The lambdas
						// are evaluated in the right environment. The lambdas are values in a property
						// list that is keyed by the name of the parameter.
					}
					final ListStruct declsOnward = list.getRest();
					// ((declare ...) body...)
					ListStruct afterDecls = declsOnward.getRest();
					// ((body...))

					// now I have to splice in any init form code before the (block foo...
					ListStruct blockCdr = NullStruct.INSTANCE;
					if ((afterDecls.getFirst() instanceof ListStruct)
							&& ((ListStruct) afterDecls.getFirst()).getFirst().equals(SpecialOperator.BLOCK)) {
						// this section used when fn was created by DEFUN
						blockCdr = ((ListStruct) afterDecls.getFirst()).getRest().getRest();
					} else {
						// push NIL
						afterDecls = new ConsStruct(NullStruct.INSTANCE, afterDecls);
//                        // push BLOCK
						afterDecls = new ConsStruct(SpecialOperator.BLOCK, afterDecls);
						afterDecls = ListStruct.buildProperList(afterDecls);
						blockCdr = ((ListStruct) afterDecls.getFirst()).getRest().getRest();
						;
					}

					final PackageSymbolStruct provideInitFormUsage = GlobalPackageStruct.COMPILER.findSymbol("PROVIDE-INIT-FORM-USAGE");
					final FunctionStruct provideInitFormUsageFn = provideInitFormUsage.getSymbolStruct().getFunction();

					if (provideInitFormUsageFn != null) {
						final ListStruct insert = (ListStruct) provideInitFormUsageFn.apply(theParsedLambdaList);
						// splice the code into the afterDecl
						if (!insert.equals(NullStruct.INSTANCE)) {
							afterDecls = (ListStruct) AppendFunction.funcall(insert, afterDecls);
//                            ((ConsStruct)funLast.funcall(insert)).setCdr(blockCdr);
//                            ((ConsStruct)((ListStruct)afterDecls.getFirst()).getRest()).setCdr(insert);
						}
						// let's see what's going with
					}
					// there may be &aux VariableOlds that get turned into a let* that starts before the
					// block construct.
					if ((auxValues != null) && !auxValues.equals(NullStruct.INSTANCE)) {
						afterDecls = new ConsStruct(SpecialOperator.LET_STAR, new ConsStruct(auxValues, afterDecls));
						afterDecls = new ConsStruct(afterDecls, NullStruct.INSTANCE);
					}

					// now splice the static-field for as the first line of the body
					((ConsStruct) declsOnward).setCdr(afterDecls);
				} else {
					while (!params.equals(NullStruct.INSTANCE)) {
						SemanticAnalyzer.addBinding_LambdaFletLabels((SymbolStruct) params.getFirst(), ++SemanticAnalyzer.bindingsPosition);
						params.setElement(1, ((ListStruct) SemanticAnalyzer.bindings.getFirst()).getFirst());
						params = params.getRest();
					}
				}
			}

			//************** Now we work on the body of the LAMBDA ***********
			// if there's an empty body, make it just a NIL
			if (list.getRest().equals(NullStruct.INSTANCE)) {
				((ConsStruct) list).setCdr(new ConsStruct(NullStruct.INSTANCE, NullStruct.INSTANCE));
			}

			// list
			list = list.getRest();
			final ListStruct listCopy = list;
			while (!list.equals(NullStruct.INSTANCE)) {
				list.setElement(1, SemanticAnalyzer.saMainLoop(list.getFirst()));
				list = list.getRest();
			}

			//set the current environment back to what it was before we hit this method
			return new ConsStruct(SemanticAnalyzer.environmentStack.peek(), listCopy);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;  //???
			SemanticAnalyzer.environmentStack.pop();
			// done with the current hack for tail-called functions
			SemanticAnalyzer.currentParsedLambdaList.pop();
			SemanticAnalyzer.currentLispName.pop();
			SemanticAnalyzer.dupSet = SemanticAnalyzer.dupSetStack.pop();
		}
	}
}
