package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.structs.arrays.ArrayStruct;
import jcl.structs.arrays.StringStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.structs.classes.StructureClassStruct;
import jcl.structs.classes.StructureObjectStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.numbers.NumberStruct;
import jcl.structs.streams.StreamStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

/**
 * Class Eval has two methods, apply() and funcall() and a private constructor.
 * Eval's funcall() implements two of these special forms, QUOTE and SETQ, and defers
 * the rest of them to the compiler.  Eval follows the function template.
 */
public class EvalFunction {

	public static final EvalFunction FUNCTION = new EvalFunction();

	private static byte recursionDepth = 0;

	/**
	 * funcall(Object arg1) takes an Object as a parameter, evaluates the
	 * Object, and returns an Object based on the evaluation.  If the Object is
	 * an instance of lisp.common.type.String or lisp.common.type.Integer,
	 * return the Object.  If the Object is an instance of
	 * lisp.common.type.Symbol return the value of the Object.  If the Object is
	 * an instance of lisp.common.type.List, check if the Car() of the List is
	 * Quote.  If so, check the list size.  If the list has any number of items
	 * other than exactly 2, throw wrongNumberOfArgsException.  Else, if the
	 * first element is Quote and the list has 2 items, return the second item
	 * unevaluated.  If the Car() is Setq, check the size of the list.  If the
	 * list has any number of items other than exactly 3, throw
	 * wrongNumberOfArgsException.  Else, check that second element is an
	 * instance of lisp.common.type.Symbol.  If not, throw
	 * invalidInputException.  If the second element is a symbol, evaluate the
	 * third item recursively (funcall(Object thirdItem)) and set the result to
	 * be the value of the second item.  If the first item is neither Quote nor
	 * Setq, check for instance of lisp.common.type.function.  If so, apply the
	 * function to the list, and return the result.
	 *
	 * @param arg1 either lisp.common.type.Symbol, lisp.common.type.List,
	 *             java.lang.String, or java.Lang.Integer
	 * @return an evaluated object
	 * @throws IllegalArgumentException An illegal argument was passed
	 */
	@SuppressWarnings("unchecked")
	public LispStruct funcall(LispStruct arg1) {
		LispStruct rtnObj = null;

		// Increment the recusrion depth.
		recursionDepth++;

		try {
			// try out Macroexpand before we do anything else
			MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(arg1);
			arg1 = macroExpandReturn.getExpandedForm();

			while (macroExpandReturn.wasExpanded()) {
				macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(arg1);
				arg1 = macroExpandReturn.getExpandedForm();
			}

			if (arg1 instanceof StringStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof CharSequence) {
				rtnObj = arg1;
			} else if (arg1 instanceof NumberStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof ArrayStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof CharacterStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof StructureClassStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof StructureObjectStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof StreamStruct) {
				rtnObj = arg1;
			} else if (arg1 instanceof SymbolStruct) {
				rtnObj = ((SymbolStruct) arg1).getFunction();
			} else if (arg1 instanceof ListStruct) {
				ListStruct list = (ListStruct) arg1;
				Object first = list.getFirst();
				ListStruct argList = list.getRest();
				int numArgs = argList.size();

				// Search the list for special symbols. If any are found, then
				// we need to compile the list. Otherwise, `funcall' can be
				// called recursively to evaluate the list.
				if (containsSpecialOperator(list)) {
					// NOTE: this should be recoded when the real COMPILE works

					// If the first thing in the list is a `lambda' then we want
					// to return the function that the compiler returns. If the
					// first thing in the list is not a `lambda' then the
					// compiler will wrap the list in a `lambda' before
					// compiling, which means that we will need invoke its
					// funcall() method and return its result.

					// Invoke the compiler.

					rtnObj = CompileFunction.FUNCTION.funcall(arg1);
					if (first != SpecialOperator.LAMBDA) {
						//System.out.println("No Lambda, funcalling");
						// The list passed in originally didn't have a lambda as
						// the first element, so the compiler wrapped it in one.
						// Evaluate the lambda function the compiler wrapped it
						// in and return the result.
						if (rtnObj == null) {
							System.out.println("Form: rtn null: " + arg1.toString().substring(0, 80) + "... compiled to null");
//                            System.out.println("... get before the env: " + ((List)((List)arg1).rest().rest().getCar()).rest().rest() );
						} else {
							FunctionStruct lambda = (FunctionStruct) rtnObj;
							if (lambda == null) {
								System.out.println("Form rtn(F0) null" + rtnObj.toString().substring(0, 80) + "... compiled to null");
//                                System.out.println("... get before the env: " + ((List)((List)arg1).rest().rest().getCar()).rest().rest() );
							}
							rtnObj = lambda.apply();
						}
					} else {
						//System.out.println("Lambda, just returning");
					}
				} else if (first instanceof SymbolStruct) {
					SymbolStruct<?> operator = (SymbolStruct) first;

					// If the element being evaluated is Quote, we do not want
					// to evaluate the rest of the list.  We simply want to
					// return the car of the cdr, or simply the rest of the
					// unevaluated list.
					if (operator == SpecialOperator.QUOTE) {
						if (numArgs == 1) {
							rtnObj = argList.getFirst();
						} else {
							throw new RuntimeException("Quote must have exactly one arg");
						}
					} // If the element being evaluated is Setq, we want to check
					// that the rest element in the list is a symbol.  If so,
					// evaluate the rest of the list and set the value of the
					// symbol to the result of the evaluation.  Else, throw an
					// exception.
					else if (operator == SpecialOperator.SETQ) {
						if (numArgs % 2 == 0) {
							while (argList != NullStruct.INSTANCE) {
								if (argList.getFirst() instanceof SymbolStruct) {
									SymbolStruct<LispStruct> operand1 = (SymbolStruct) argList.getFirst();
									operand1.setValue(funcall(argList.getRest().getFirst()));
									rtnObj = operand1.getValue();
								} else {
									throw new IllegalArgumentException(
											"First argument must be a Symbol. "
													+ argList.getFirst());
								}
								argList = argList.getRest().getRest();
							}
						} else {
							throw new RuntimeException("SetQ must have even number of args");
						}
					} // If the element is not a special form, a recursive call is
					// made to evaluate the rest of the list.
					else {
						Object maybeFunction = operator.getFunction();

						if (maybeFunction instanceof FunctionStruct) {
							FunctionStruct function = (FunctionStruct) maybeFunction;
							// evaluate the arguments left to right
							boolean isBaseMunger = false;
							try {
								isBaseMunger = SemanticAnalyzer.LAMBDA_ARGLIST_MUNGER == function.getClass().getField("LAMBDA_ARGLIST_MUNGER").get(null);
							} catch (Exception ex) {
								isBaseMunger = true;
							}
							if (isBaseMunger) {
								// if here, then it's all required args and can be simply eval'ed
								ListStruct newArgList = NullStruct.INSTANCE;
								while (argList != NullStruct.INSTANCE) {
									newArgList = new ConsStruct(funcall(argList.getFirst()), newArgList);
									argList = argList.getRest();
								}
								newArgList = NReverseFunction.funcall(newArgList);
								rtnObj = function.apply(newArgList);
							} else {
								// now we have to compile it to handle &optional, &rest, &key args, and then funcall it
								// But first, a little foo-foo...
								ListStruct listifiedList = ListStruct.buildProperList(list);
								ListStruct formToCompile = new ConsStruct(SpecialOperator.LAMBDA, new ConsStruct(NullStruct.INSTANCE, listifiedList));
								// TODO: when the new compile works
								FunctionStruct compiledFn = (FunctionStruct) CompileFunction.FUNCTION.funcall(formToCompile);
								rtnObj = compiledFn.apply();
							}
						}
					}
				}
			} else {
				System.out.println("Eval: a form I can't evaluate, ~A" + arg1);
				rtnObj = arg1;
			}

			return rtnObj;
		} finally {
			--recursionDepth;
		}
	}

	private boolean containsSpecialOperator(ListStruct list) {
		//System.out.println("has specialop?: " + list);
		boolean retval = false;
		if (list == NullStruct.INSTANCE) {
			return false;
		}
		Object theCar = list.getFirst();

		// check the first element of the list
		if (theCar instanceof SpecialOperator) {
			// its any one of the special operators, but if it's QUOTE or SETQ
			// they get handled specially
			if (theCar == SpecialOperator.QUOTE) {
				// the rest thing is completely quoted, so cadr doesn't need
				// to be examined
				return false;
			} else if (theCar == SpecialOperator.SETQ) {
				// ******** this is not correct!!!, fix later

				// if SETQ, then skip the rest element and check the 2nd arg
				// since it is evaluated
				theCar = list.getRest().getRest().getFirst();
				if (theCar instanceof ListStruct) {
					return containsSpecialOperator((ListStruct) theCar);
				} else {
					return false;
				}
			} else {
				return true;
			}
		} else if (theCar instanceof ListStruct) {
			return containsSpecialOperator((ListStruct) theCar)
					|| containsSpecialOperator(list.getRest());
		}
		// run the rest of the list
		if (((ConsStruct) list).getCdr() instanceof ListStruct) {
			list = list.getRest();
			while (list != NullStruct.INSTANCE) {
				theCar = list.getFirst();
				if ((theCar instanceof ListStruct)
						&& (containsSpecialOperator((ListStruct) theCar))) {
					return true;
				}
				if (((ConsStruct) list).getCdr() instanceof ListStruct) {
					list = list.getRest();
				} else {
					break;
				}
			}
		}
		return false;
	}
}
