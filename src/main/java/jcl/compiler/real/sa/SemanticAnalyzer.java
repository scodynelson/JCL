package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.StringStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.WrapInLambda;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.sa.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.specialoperator.QuoteAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Declaration;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Stack;
import java.util.Vector;
import java.util.regex.Pattern;

public class SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzer.class);

	public static final SymbolStruct Defmacro = GlobalPackageStruct.COMMON_LISP.findSymbol("DEFMACRO").getSymbolStruct();
	public static final SymbolStruct COMPILE_TOPLEVEL = KeywordOld.CompileToplevel;
	public static final SymbolStruct LOAD_TOPLEVEL = KeywordOld.LoadToplevel;
	public static final StringStruct LAMBDA_ARGLIST_MUNGER_STRING = new StringStruct("LAMBDA_ARGLIST_MUNGER");

	// eval-when processing modes
	private boolean topLevelMode;

	static String nameBreakingRegex = "[^\\p{Alnum}]";
	static Pattern nameBreakingPattern = Pattern.compile(nameBreakingRegex);
	public static ListStruct bindings;
	public static Stack<Environment> environmentStack;
	public static Stack<SymbolStruct> blockStack;
	public static Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack;
	public static int iTagbodyCounter;
	public static Vector<SymbolStruct> undefinedFunctions;
	public static Stack<ListStruct> currentParsedLambdaList;
	public static Stack<SymbolStruct> currentLispName;
	public static Stack<IdentityHashMap<LispStruct, LispStruct>> dupSetStack;
	public static IdentityHashMap<LispStruct, LispStruct> dupSet = null;
	public static int bindingsPosition;
	// and association of function names seen and their arglist munging
	// used to handle recursive functions
	public static ListStruct currentArgMunger = NullStruct.INSTANCE;

	private void initialize() {
		//create the global environment
		environmentStack = new Stack<>();
		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());
		currentParsedLambdaList = new Stack<>();
		currentParsedLambdaList.push(NullStruct.INSTANCE);
		currentLispName = new Stack<>();
		currentLispName.push(null);

		topLevelMode = true;

		blockStack = new Stack<>();
		tagbodyStack = new Stack<>();
		iTagbodyCounter = 0;
		undefinedFunctions = new Vector<>();
		bindingsPosition = 0;
		bindings = NullStruct.INSTANCE;
		dupSetStack = new Stack<>();
	}

	public LispStruct funcall(LispStruct form) {
		initialize();
		if (!(form instanceof ListStruct)
				|| !(((ListStruct) form).getFirst() instanceof SymbolStruct)
				|| !(((ListStruct) form).getFirst().equals(SpecialOperator.LAMBDA)
				|| ((ListStruct) form).getFirst().equals(SpecialOperator.MACRO_LAMBDA))) {
			form = new WrapInLambda().funcall(form);
		}
		// make a copy so we can trash it in SA
		form = saMainLoop(form);
		// now setup the closure depths
		form = saSetClosureDepth(form, 0);
		// clear the dup hash map

		// now see if we have any functions still undefined
		Iterator<SymbolStruct> iterator = undefinedFunctions.iterator();
		while (iterator.hasNext()) {
			if (iterator.next().getFunction() != null) {
				// one with a fn, drop it
				iterator.remove();
			}
		}
		// now print out the ones outstanding
		iterator = undefinedFunctions.iterator();
		while (iterator.hasNext()) {
			final SymbolStruct fnName = iterator.next();
			LOGGER.warn("; Warning: no function or macro function defined for ");
			if (fnName.getSymbolPackage() != null) {
				LOGGER.warn("{}::{}", fnName.getSymbolPackage().getName(), fnName.getName());
			} else {
				LOGGER.warn("#:{}", fnName.getName());
			}
		}
		return form;
	}

	public static LispStruct saMainLoop(LispStruct form) {
		if (form.equals(NullStruct.INSTANCE)) {
			// do nothing
		} else if (form instanceof KeywordSymbolStruct) {
			//do nothing
		} else if (form instanceof CharacterStruct) {
			//do nothing
		} else if (form instanceof NumberStruct) {
			//do nothing
		} else if (form instanceof SymbolStruct) {
			form = SymbolStructAnalyzer.INSTANCE.analyze((SymbolStruct<?>) form);
		} else if (form instanceof ListStruct) {
			form = ListStructAnalyzer.INSTANCE.analyze((ListStruct) form);
		} else if (form instanceof VectorStruct) {
			form = VectorStructAnalyzer.INSTANCE.analyze((VectorStruct) form);
		} else if (form instanceof ArrayStruct) {
			form = ArrayStructAnalyzer.INSTANCE.analyze((ArrayStruct) form);
		} else {
			LOGGER.warn("SA: Found thing I can't semantically analyze: {}, class: {}", form, form.getClass().getName());
		}
		return form;
	}

	/*
	 *********************************************************
	 * Analyzers
	 *********************************************************
	 */

	/*
	 *********************************************************
	 * Special Operator Analyzers
	 *********************************************************
	 */

	public static Map<SymbolStruct<?>, SymbolStruct<?>> getFunctionNames(final String functionBinding, final ListStruct listStruct,
																		 final List<LispStruct> functionJavaList) {
		final Map<SymbolStruct<?>, SymbolStruct<?>> functionNameMap = new HashMap<>();

		for (final LispStruct currentFunction : functionJavaList) {
			if (!(currentFunction instanceof ListStruct)) {
				throw new RuntimeException("Improperly formed " + functionBinding + ": " + listStruct);
			}

			final ListStruct functionListStruct = (ListStruct) currentFunction;
			final LispStruct functionFirst = functionListStruct.getFirst();
			if (!(functionFirst instanceof SymbolStruct)) {
				throw new RuntimeException("Improperly formed " + functionBinding + ": " + listStruct);
			}

			final SymbolStruct<?> functionName = (SymbolStruct) functionFirst;
			final SymbolStruct<?> gensymFunctionName = GensymFunction.funcall(functionName.getName());
			functionNameMap.put(functionName, gensymFunctionName);
		}

		return functionNameMap;
	}

	/*
	 *********************************************************
	 * OTHER STUFF
	 *********************************************************
	 */


	private int saResetBindingSlots(final ListStruct theEnv, int depth) {
		// get the binding list from the environment
		ListStruct bindingList = AssocFunction.funcall(KeywordOld.Bindings, theEnv.getRest()).getRest();
		// now run through them and incrementing the depth
		while (!bindingList.equals(NullStruct.INSTANCE)) {
			// get the :allocation parameter - (:LOCAL . n)
			ListStruct bindingElement = (ListStruct) bindingList.getFirst();
			// (x :scope ... :allocation ...)
			bindingElement = bindingElement.getRest();
			final ConsStruct lclSlot = (ConsStruct) GetPlist.funcall(bindingElement,
					KeywordOld.Allocation);
			lclSlot.setCdr(new IntegerStruct(BigInteger.valueOf(depth++)));
			bindingList = bindingList.getRest();
		}
		return depth;
	}

	private int saResetFreeSlots(final ListStruct theEnv, int depth) {
		// get the SymbolStruct table list from the environment
		ListStruct symbolTable = AssocFunction.funcall(KeywordOld.SymbolTable, theEnv.getRest()).getRest();
		// now run through them and incrementing the depth
		while (!symbolTable.equals(NullStruct.INSTANCE)) {
			// get the :allocation parameter - (:LOCAL . n)
			ListStruct tableElement = (ListStruct) symbolTable.getFirst();
			// (x :scope ... :allocation ...)
			tableElement = tableElement.getRest();
			final ConsStruct lclSlot = (ConsStruct) GetPlist.funcall(tableElement,
					KeywordOld.Allocation);
			// have to make sure it's allocated locally
			if (lclSlot.getFirst().equals(KeywordOld.Local)) {
				lclSlot.setCdr(new IntegerStruct(BigInteger.valueOf(depth++)));
			}
			symbolTable = symbolTable.getRest();
		}
		return depth;
	}

	private Object saResetLocals(final Object form, int depth) {
		// go through all forms looking for bindings and reseting the
		// local slot allocations. On encountering a %lambda, the
		// counter is reset to 1 (first parameter). The sequence is
		// to reallocate lexical bindings, then reset the allocations for
		// special VariableOlds
		if (!form.equals(NullStruct.INSTANCE)) {
			if (form instanceof ListStruct) {
				final ListStruct workingList = (ListStruct) form;
				// this may be the start of a lambda or let expr
				// maybe ((system::%lambda (:parent ...) (:bindings...) ...) ...)
				final Object theCar = workingList.getFirst();
				if (theCar instanceof ListStruct) {
					// (system::%lambda (:parent ...) (:bindings...) ...)
					final Object test = ((ListStruct) theCar).getFirst();
					if (test.equals(SpecialOperator.LAMBDA_MARKER) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.MACRO_MARKER)) {
						if (!test.equals(SpecialOperator.LET)) {
							//reset the counter
							depth = 1;
						}
						final ListStruct theEnv = (ListStruct) theCar;
						depth = saResetBindingSlots(theEnv, depth);
						depth = saResetFreeSlots(theEnv, depth);
					}
					saResetLocals(workingList.getRest(), depth + 20); // just in case there are locals allocated
				} else {
					saResetLocals(theCar, depth);
					saResetLocals(workingList.getRest(), depth);
				}
			}
		}
		return form;
	}

	private LispStruct saSetClosureDepth(final LispStruct form, int depth) {
		if (!form.equals(NullStruct.INSTANCE)) {
			if (form instanceof ListStruct) {
				final ListStruct workingList = (ListStruct) form;
				// this may be the start of a lambda or let expr
				final LispStruct theCar = workingList.getFirst();
				if (theCar instanceof ListStruct) {
					final Object test = ((ListStruct) theCar).getFirst();
					if (test.equals(SpecialOperator.LAMBDA_MARKER) || test.equals(SpecialOperator.LET) || test.equals(SpecialOperator.LABELS) || test.equals(SpecialOperator.FLET) || test.equals(SpecialOperator.MACRO_MARKER)) {
						final ListStruct theEnv = (ListStruct) theCar;
						// it is, so see if there's a closure defined
						// Get the current closure entry
						final ConsStruct closure = (ConsStruct) AssocFunction.funcall(
								KeywordOld.Closure, theEnv.getRest());
						// add the depth indicator
						// (rplacd closure (cons (cons :depth depth) (cdr closure)))
						// there may be a depth gauge...
						final ListStruct depthGauge = AssocFunction.funcall(KeywordOld.Depth, closure.getRest());
						if (depthGauge.equals(NullStruct.INSTANCE)) { // it may have been handled as a labels
							if (!closure.getRest().equals(NullStruct.INSTANCE)) {
								depth++;
							}
							closure.setCdr(
									new ConsStruct(
											new ConsStruct(
													KeywordOld.Depth, new IntegerStruct(BigInteger.valueOf(depth))),
											closure.getCdr()));
						}
						// walk the body of the lambda or let
						saSetClosureDepth(workingList.getRest(), depth);
					} else {
						saSetClosureDepth(theCar, depth);
						saSetClosureDepth(workingList.getRest(), depth);
					}
				} else {
					if (!theCar.equals(SpecialOperator.DECLARE)) {
						saSetClosureDepth(workingList.getRest(), depth);
					}
				}
			}
		}
		return form;
	}

	public static FunctionStruct LAMBDA_ARGLIST_MUNGER =
			new FunctionStruct() {

				public LispStruct funcall(final LispStruct arglist, final LispStruct marker) {
					return new ConsStruct(SpecialOperator.FUNCTION_MARKER,
							new ConsStruct(marker, arglist));
				}

				@Override
				public LispStruct apply(final LispStruct... lispStructs) {
					return funcall(lispStructs[0], lispStructs[1]);
				}
			};

	// insert from LinL?

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
				currentLispName.push(null);
				final String name = "AnonymousLambda_" + System.currentTimeMillis() + '_';
				classname = (SymbolStruct) GensymFunction.funcall(name);
			} else {
				final SymbolStruct lispName = (SymbolStruct) lispNameDecl.getRest().getFirst();
				classname = (SymbolStruct) GensymFunction.funcall(javafy(lispName) + System.currentTimeMillis());
				// this code allows saFunctionCall to recognize a recursive call
				// the pop happens at the end of saLambda
				currentLispName.push(lispName);
			}
			newDecls = new ConsStruct(new ConsStruct(Declaration.JAVA_CLASS_NAME, ListStruct.buildProperList(classname)), newDecls);
		}
		currentLispName.push(null);
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

	public static ListStruct findDeclaration(final SymbolStruct item, final ListStruct list) {
		return AssocFunction.funcall(item, list);
	}

	// New helper function. It takes a parsedLambdaList and removes a FakeRest used for setting up the lambda
	// list for function application
	public static ListStruct removeFakeRestEntry(final ListStruct parsedLambdaList) {
		// Only do nothing unless the parsedLambdaList has a FakeRest
		ListStruct thePLL = parsedLambdaList;
		final SymbolStruct fakeRestSymbolStruct = GlobalPackageStruct.COMPILER.findSymbol(
				"FakeRestSymbolStructForHandlingKeysWithout&Rest").getSymbolStruct();
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
				"FakeRestSymbolStructForHandlingKeysWithout&Rest").getSymbolStruct();
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

	public static SymbolStruct saGetClassName(ListStruct list) {
		// check to see if any declarations exist
		if (list.getFirst().equals(SpecialOperator.DECLARE)) {
			// strip out DECLARATION keyword
			list = list.getRest();
			return (SymbolStruct) AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, list).getRest().getFirst();
		}
		return null;
	}

	private void addBinding_FBinding(final SymbolStruct sym, final Object initForm, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewFBinding(environmentStack.peek(), sym, position,
				(SymbolStruct) GensymFunction.funcall(javafy(sym) + System.currentTimeMillis()), isSpecial);
	}

	public static void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position) {
		addBinding_LambdaFletLabels(sym, position, false);
	}

	private static void addBinding_LambdaFletLabels(final SymbolStruct sym, final int position, final boolean isSpecial) {
		EnvironmentAccessor.createNewLambdaBinding(environmentStack.peek(), sym, position, isSpecial);
	}
}
