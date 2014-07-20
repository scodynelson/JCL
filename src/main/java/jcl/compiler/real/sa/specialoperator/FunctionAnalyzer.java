package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.CompileFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.functions.XCopyTreeFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.MacroFunctionBinding;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.compiler.real.sa.specialoperator.compiler.StaticFieldAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import java.util.regex.Pattern;

public class FunctionAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionAnalyzer INSTANCE = new FunctionAnalyzer();

	public static ListStruct bindings;

	private static String nameBreakingRegex = "[^\\p{Alnum}]";
	private static Pattern nameBreakingPattern = Pattern.compile(nameBreakingRegex);

	public static final StringStruct LAMBDA_ARGLIST_MUNGER_STRING = new StringStruct("LAMBDA_ARGLIST_MUNGER");

	@Override
	public ListStruct analyze(final ListStruct input) {
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
				final MacroFunctionBinding macroFunctionBinding = (MacroFunctionBinding) fnBinding.getBinding(functionSymbol);
				final SymbolStruct<?> functionBindingName = macroFunctionBinding.getName();

				final LispStruct first = input.getFirst();
				return new ConsStruct(first, functionBindingName);
			}
		} else if (second instanceof ListStruct) {
			final ListStruct functionList = (ListStruct) second;

			final LispStruct functionListFirst = functionList.getFirst();
			if (functionListFirst.equals(SpecialOperator.LAMBDA)) {
				final Environment newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LAMBDA);
				final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();
				EnvironmentAccessor.createParent(newEnvironment, parentEnvironment);
				SemanticAnalyzer.environmentStack.push(newEnvironment);

				return saLambdaAux(functionList, newEnvironment);
			} else if (functionListFirst.equals(SpecialOperator.MACRO_LAMBDA)) {
				final Environment newEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACRO);
				final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();
				EnvironmentAccessor.createParent(newEnvironment, parentEnvironment);
				SemanticAnalyzer.environmentStack.push(newEnvironment);

				return saLambdaAux(functionList, newEnvironment);
			}

			if (functionListFirst.equals(GlobalPackageStruct.COMMON_LISP.findSymbol("SETF").getSymbolStruct())) {
				return input; // no changes at this level
			}

			throw new RuntimeException("Improperly Formed Function: if the first argument is a ListStruct, it must be a LAMBDA");
		}

		throw new RuntimeException("Improperly Formed Function: the arguments must be either a ListStruct or SymbolStruct");
	}

	private static ListStruct saLambdaAux1(final ListStruct listStruct, final Environment environment, final boolean isLambda) {
		//might also need here prev. bindings position num, or even a stack?
		final int tempPosition = SemanticAnalyzer.bindingsPosition;

		// keep track of the current environment as it is "now"
		// set the current environment's parent to what was the environment
		try {
			final LispStruct firstElement = listStruct.getFirst();
			if (!(firstElement instanceof SymbolStruct)) {
				// TODO: This can really go away, as we build this list above anyways...
				throw new RuntimeException("First argument to Lambda must be the SymbolStruct 'Lambda'");
			}

			final LispStruct secondElement = listStruct.getRest().getFirst();
			if (!(secondElement instanceof ListStruct)) {
				throw new RuntimeException("Second argument to Lambda must be a ListStruct of parameters");
			}

			final ListStruct parameters = (ListStruct) secondElement;
			final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(parameters);

			// TODO: the old code did this whole "ArgListMunging" crap. Basically, it adds a function dynamically
			// TODO: to the function being created that analyzes the arguments at runtime. so that when it gets called,
			// TODO: the required arguments get checked to validate that they were in fact supplied. Now, how do we do
			// TODO: this in the new version???

			final ListStruct declaresDocStringBody = listStruct.getRest().getRest();
			final ListStruct orderedDeclaresDocStringBody = saDeclarations(declaresDocStringBody);

			// TODO: the old code did this whole "ArgInitFormUsage" stuff. It basically does the work that determines
			// TODO: if the 'supplied-p' parameter has a values of T or NIL. What it will do is 'inline' an "if" block that
			// TODO: will dynamically check the input of the variable and then "SETQ" the value of the 'supplied-p' parameter.

			return null;
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;  //???
			SemanticAnalyzer.environmentStack.pop();
			SemanticAnalyzer.currentLispName.pop();
		}
	}

	private static ListStruct saLambdaAux(final ListStruct list, final Environment newEnvironment) {
		//might also need here prev. bindings position num, or even a stack?
		final int tempPosition = SemanticAnalyzer.bindingsPosition;

		// keep track of the current environment as it is "now"
		// set the current environment's parent to what was the environment
		try {
			// list => (%lambda (lambda-list) (declare ...) ?"...doc..." body...)
			final SymbolStruct lambdaSym = (SymbolStruct) list.getFirst(); // hang on to %lambda or %macro

			// list -> ((lambda-list) ?(declare ...) ?"...doc..." body...)
			// get the parameter list
			ListStruct params = (ListStruct) list.getRest().getFirst();
			// params -> (lambda-list)

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
						bindingList.setCdr(removeFakeRestEntry(theParsedLambdaList));
					}
				}
			}
			// temporary hack to handle tail-called functions here
//			currentParsedLambdaList.push(theParsedLambdaList);

			// classname is a declaration in the declare form
			// if there isn't one, it makes one up

			// list -> (?(declare ...)  ?"...doc..."body...)
			// handle any declarations and doc string that might be present
			// returns an updated list with at least one DECLARATION
			// doc string, if present, is now in a declaration
			final ListStruct listWithOrderedDeclarationsAndDocString = saDeclarations(list.getRest().getRest());
			// list -> ((declare ...) body...)
			final ListStruct decls = (ListStruct) listWithOrderedDeclarationsAndDocString.getFirst();
			// decls -> (declare ...)
			final SymbolStruct classname = saGetClassName(decls);

			// now reconstitute the full lambda form
			// list => (%lambda (lambda-list) (declare ...) body...)

			// step over the lambda SymbolStruct to work on the params etc

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
							&& findDeclaration(Declaration.NO_GENERATE_ANALYZER, decls.getRest()).equals(NullStruct.INSTANCE)) {
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
						StaticFieldAnalyzer.INSTANCE.analyze(ListStruct.buildProperList(NullStruct.INSTANCE, analyzerFn, LAMBDA_ARGLIST_MUNGER_STRING));
						// this static field holds lambdas that provide init values. The lambdas
						// are evaluated in the right environment. The lambdas are values in a property
						// list that is keyed by the name of the parameter.
					}
					final ListStruct declsOnward = listWithOrderedDeclarationsAndDocString;
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
						EnvironmentAccessor.createNewLambdaBinding(SemanticAnalyzer.environmentStack.peek(), (SymbolStruct) params.getFirst(), ++SemanticAnalyzer.bindingsPosition, false);
						params.setElement(1, ((ListStruct) bindings.getFirst()).getFirst());
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

			List<LispStruct> bodyElements = list.getRest().getAsJavaList();
			List<LispStruct> newBodyElements = new ArrayList<>();
			for (final LispStruct element : bodyElements) {
				newBodyElements.add(SemanticAnalyzer.saMainLoop(element));
			}

			// TODO: now rebuild the list again....

			//set the current environment back to what it was before we hit this method
			return new ConsStruct(SemanticAnalyzer.environmentStack.peek(), list);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;  //???
			SemanticAnalyzer.environmentStack.pop();
			SemanticAnalyzer.currentLispName.pop();
		}
	}

	public static ListStruct findDeclaration(final SymbolStruct item, final ListStruct list) {
		return AssocFunction.funcall(item, list);
	}

	// New helper function. It takes a parsedLambdaList and removes a FakeRest used for setting up the lambda
	// list for function application
	public static ListStruct removeFakeRestEntry(final ListStruct parsedLambdaList) {
		// Only do nothing unless the parsedLambdaList has a FakeRest
		ListStruct thePLL = parsedLambdaList;
		final SymbolStruct fakeRestSymbolStruct = GlobalPackageStruct.COMPILER.findSymbol(
				"FakeRestSymbolForHandlingKeysWithout&Rest").getSymbolStruct();
		while (!thePLL.equals(NullStruct.INSTANCE)) {
			// if we find a fakeRestSymbolStruct at the beginning of a lambda list, remove it
			if (((ListStruct) thePLL.getFirst()).getFirst().equals(fakeRestSymbolStruct)) {
				// found one!
				return removeFakeRestEntryAux(parsedLambdaList);
			} else {
				thePLL = thePLL.getRest();
			}
		}
		return parsedLambdaList;
	}

	private static ListStruct removeFakeRestEntryAux(final ListStruct parsedLambdaList) {
		// doesn't use the Collection framework remove because the check has to be inside the cons
		// So, we do it the hard way
		final List<LispStruct> copyJavaList = parsedLambdaList.getAsJavaList();
		final boolean isPLLDotted = parsedLambdaList.isDotted();

		final ListStruct copyParsedLambdaList;
		if (isPLLDotted) {
			copyParsedLambdaList = ListStruct.buildDottedList(copyJavaList);
		} else {
			copyParsedLambdaList = ListStruct.buildProperList(copyJavaList);
		}

		final SymbolStruct fakeRestSymbolStruct = GlobalPackageStruct.COMPILER.findSymbol(
				"FakeRestSymbolForHandlingKeysWithout&Rest").getSymbolStruct();
		// Check to see if the first element is the fake. This happens in the case of
		// of the param list starts with &key
		final ListStruct parsedLambdaElt = (ListStruct) copyParsedLambdaList.getFirst();
		if (parsedLambdaElt.getFirst().equals(fakeRestSymbolStruct)) {
			// just return the rest of the parsed lambda elements
			return adjustParameterNbrs(copyParsedLambdaList.getRest()); // NOTE: decr the parameter
		}
		// here the fake rest is embedded
		ListStruct prior = copyParsedLambdaList;
		ListStruct current = copyParsedLambdaList.getRest();
		// see if we have any
		while (!current.equals(NullStruct.INSTANCE)) {
			if (((ListStruct) current.getFirst()).getFirst().equals(fakeRestSymbolStruct)) {
				// now splice it out
				// now we spice out the fake rest entry
				((ConsStruct) prior).setCdr(current.getRest());
				// now adjust the parameters that are post- fake element
				adjustParameterNbrs(current.getRest());
				// now drop the first element to the garbage
				((ConsStruct) current).setCdr(NullStruct.INSTANCE);
				break;
				// now we have to decr the parameter parameter (really) to account for no param slot for the fake
			}
			prior = prior.getRest();
			current = current.getRest();
		}
		return parsedLambdaList;
	}

	private static ListStruct adjustParameterNbrs(final ListStruct paramsToAdjust) {
		final SymbolStruct allocKey = GlobalPackageStruct.KEYWORD.findSymbol("ALLOCATION").getSymbolStruct();
		// Now we have to rework the parameter indices since we just dropped a paramter (the fake rest)
		ListStruct adjustableList = paramsToAdjust;
		while (!adjustableList.equals(NullStruct.INSTANCE)) {
			// for each &key params, get the parameter number, decr it, and put it back
			final ListStruct params = ((ListStruct) adjustableList.getFirst()).getRest(); // now we have a plist
			// get the parameter cons entry and decr the cdr number
			// now let's print it
			final ConsStruct theParamAlloc = (ConsStruct) GetPlist.funcall(params,
					allocKey);
			// now adjust the number found in the cdr
			final int paramNbr = ((IntegerStruct) theParamAlloc.getCdr()).getBigInteger().intValue();
			// decr the number by 1
			theParamAlloc.setCdr(new IntegerStruct(BigInteger.valueOf(paramNbr - 1)));
			adjustableList = adjustableList.getRest();
		}
		return paramsToAdjust;
	}

	private static SymbolStruct saGetClassName(ListStruct list) {
		// check to see if any declarations exist
		if (list.getFirst().equals(SpecialOperator.DECLARE)) {
			// strip out DECLARATION keyword
			list = list.getRest();
			return (SymbolStruct) AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, list).getRest().getFirst();
		}
		return null;
	}

	/**
	 * This method parses the declaration specifications - which may also
	 * hold a documentation string. All of the declarations are merged into
	 * a single DECLARE form. The doc string becomes another DECLARE option.
	 * The BNF is
	 * declarations := declSpec* docString? declSpec*
	 * declSpec := '(' DECLARE declaration* ')'
	 * declaration := '(' declName declArgs ')'
	 * <p>
	 * If there is nothing after the string, then the string is the form
	 * Not the doc string
	 */
	public static ListStruct saDeclarations(ListStruct list) {
		// list is the rest of the forms after the arg list
		ListStruct newDecls = NullStruct.INSTANCE;   //the list of all new Declarations
		final ListStruct returnList = list;

		while (!list.equals(NullStruct.INSTANCE)) {
			LispStruct theCar = list.getFirst();
			ListStruct theDecl;    //the current new Declaration
			if (theCar instanceof CharSequence) {
				// This may be a doc string, unless it's the last thing
				if (list.getRest().equals(NullStruct.INSTANCE)) {
					break;
				} else {
					// there's stuff after the doc string so it is doc
					theDecl = ListStruct.buildProperList(ListStruct.buildProperList(Declaration.DOCUMENTATION, theCar));
					newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
					list = list.getRest();

					// add code to look for more declarations - without any strings
					theCar = list.getFirst();
					while ((theCar instanceof ListStruct)
							&& ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
						theDecl = ((ListStruct) theCar).getRest();
						newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
						list = list.getRest();
						theCar = list.getFirst();
					}
					// saDeclarationsHelp will complete the search for declarations
					break;
				}
			}
			if ((theCar instanceof ListStruct) && ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
				theDecl = ((ListStruct) theCar).getRest();
				newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
			} else {
				break;
			}
			list = list.getRest();
		}

		// if there isn't a Declaration.JAVA_CLASS_NAME, add one
		final ListStruct classNameDecl = AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, newDecls);
		if (classNameDecl.equals(NullStruct.INSTANCE)) {
			final SymbolStruct classname;
			// if there's a Lisp Name, then Javafy it
			final ListStruct lispNameDecl = AssocFunction.funcall(Declaration.LISP_NAME, newDecls);
			if (lispNameDecl.equals(NullStruct.INSTANCE)) {
				SemanticAnalyzer.currentLispName.push(null);
				final String name = "AnonymousLambda_" + System.currentTimeMillis() + '_';
				classname = (SymbolStruct) GensymFunction.funcall(name);
			} else {
				final SymbolStruct lispName = (SymbolStruct) lispNameDecl.getRest().getFirst();
				classname = (SymbolStruct) GensymFunction.funcall(javafy(lispName) + System.currentTimeMillis());
				// this code allows saFunctionCall to recognize a recursive call
				// the pop happens at the end of saLambda
				SemanticAnalyzer.currentLispName.push(lispName);
			}
			newDecls = new ConsStruct(new ConsStruct(Declaration.JAVA_CLASS_NAME, ListStruct.buildProperList(classname)), newDecls);
		}
		SemanticAnalyzer.currentLispName.push(null);
		// if the value is a String, make it into a SymbolStruct
		final Object name = classNameDecl.getRest().getFirst();
		if (name instanceof CharSequence) {
			final ListStruct rest = classNameDecl.getRest();
			rest.setElement(1, new SymbolStruct(name.toString()));
		}
		// now we conjure a new declaration
		return new ConsStruct(new ConsStruct(SpecialOperator.DECLARE, newDecls), list);
	}

	// routine to javafy a Lisp SymbolStruct or string to a Java identifier
	private static String javafy(final Object lispName) {
		final StringBuilder result = new StringBuilder();
		final Scanner scanner = new Scanner(lispName.toString()).useDelimiter(nameBreakingPattern);

		while (scanner.hasNext()) {
			String fragment = scanner.next();
			if ((fragment == null) || fragment.isEmpty()) {
				fragment = "__";
			}
			final int codePoint = Character.toUpperCase(fragment.codePointAt(0));
			result.appendCodePoint(codePoint);
			result.append(fragment.substring(1).toLowerCase());
		}
		if (result.length() == 0) {
			result.append("UnknownLispName");
		}
		return result.toString();
	}
}
