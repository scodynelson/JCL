package jcl.compiler.old;

import jcl.LispStruct;
import jcl.compiler.old.functions.AconsFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.ConsFunction;
import jcl.compiler.old.functions.GetPlist;
import jcl.compiler.old.functions.SetPlist;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.old.symbol.SpecialOperatorOld;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;

import java.math.BigInteger;

public class EnvironmentAccessor {

	public static final SymbolStruct LAMBDA = SpecialOperatorOld.LAMBDA_MARKER;
	public static final SymbolStruct MACRO = SpecialOperatorOld.MACRO_MARKER;
	public static final SymbolStruct MACROLET = SpecialOperatorOld.MACROLET;
	public static final SymbolStruct LET = SpecialOperatorOld.LET_MARKER;
	public static final SymbolStruct FLET = SpecialOperatorOld.FLET_MARKER;
	public static final SymbolStruct LABELS = SpecialOperatorOld.LABELS_MARKER;
	static AconsFunction acons = AconsFunction.FUNCTION;
	static AssocFunction assoc = AssocFunction.FUNCTION;
	static ConsFunction cons = ConsFunction.FUNCTION;
	static SetPlist set = SetPlist.FUNCTION;
	static GetPlist get = GetPlist.FUNCTION;

	/**
	 * Creates a new instance of EnvironmentAccessor
	 */
	private EnvironmentAccessor() {
	}

	/**
	 * Creates a new reference environment for storing binding information
	 *
	 * @param lambdaLet either LAMBDA or LET depending on type of environment
	 * @return the new reference environment
	 */
	@SuppressWarnings("unchecked")
	public static ListStruct createNewEnvironment(SymbolStruct lambdaLet) {
		ListStruct[] assocArray = null;
//        if ((lambdaLet == FLET) || (lambdaLet == LABELS)) {
//            assocArray = new List[]{
//                        List.Factory.newInstance(new Object[]{KeywordOld.Parent}),
//                        List.Factory.newInstance(new Object[]{KeywordOld.Bindings})};
//        } else {
		assocArray = new ListStruct[]{
				ListStruct.buildProperList(KeywordOld.Parent),
				ListStruct.buildProperList(KeywordOld.Bindings),
				ListStruct.buildProperList(KeywordOld.SymbolTable),
				ListStruct.buildProperList(KeywordOld.Closure)};
//        }

		ListStruct newList = ListStruct.buildProperList(assocArray);
		// now we have place for static initializers for LOAD-TIME-VALUE
		if ((lambdaLet == LAMBDA) || (lambdaLet == MACRO) || (lambdaLet == FLET) || (lambdaLet == LABELS)) {
			newList = new ConsStruct(ListStruct.buildProperList(KeywordOld.LoadTimeValue), newList);
		}
		newList = new ConsStruct(lambdaLet, newList);

		// set parent to NIL as the default value
		newList = createParent(newList, NullStruct.INSTANCE);
		return newList;
	}
	/*
     * (defun create-new-environment (marker)
     *   (let ((new-list `((:parent nil) (:bindings) (:symbol-table) (:closure))))
     *     (cons marker
     *       (case marker
     *         ((lambda macro flet labels)
     *          (setq new-list (cons `(:load-time-value)))
     *         (t new-list)))))
     */

	/**
	 * Creates a parent reference for the current reference environment
	 *
	 * @param currentEnvironment the reference environment to which the parent is to be added
	 * @param parent             the lambda reference that is the parent of the current reference environment
	 * @return the new reference environment
	 */
	@SuppressWarnings("unchecked")
	public static ListStruct createParent(ListStruct currentEnvironment, ListStruct parent) {
		ListStruct assocList = (ListStruct) assoc.funcall(KeywordOld.Parent, currentEnvironment.getRest());
		((ConsStruct) assocList).setCdr(cons.funcall(parent, NullStruct.INSTANCE));
		return currentEnvironment;
	}

	/**
	 * Creates an environment with a NIL parent. This is equivalent to the global
	 * environment. This is used at the beginning of semantic analysis and anywhere a
	 * global environment is needed, for example, load-time-value.
	 */
	public static ListStruct createGlobalEnvironment() {
		return createParent(createNewEnvironment(LAMBDA), NullStruct.INSTANCE);
	}

    /*
     * (defun create-global-environment ()
     *   (create-new-environment 'lambda))
     */

	/**
	 * Returns the current reference environment's parent environment
	 *
	 * @param currentEnvironment the reference environment from which the parent is to be acquired
	 * @return the parent reference environment
	 */
	public static ListStruct getParent(ListStruct currentEnvironment) {
		ListStruct assocList = (ListStruct) assoc.funcall(KeywordOld.Parent, currentEnvironment.getRest());
		return (ListStruct) (assocList.getRest().getFirst());
	}

	/**
	 * (defun get-parent (curr-env)
	 * (second (assoc :parent (rest curr-env)))
	 */
	@SuppressWarnings("unchecked")
	public static ListStruct createNewLambdaBinding(ListStruct currentEnvironment, SymbolStruct newVariable, IntegerStruct position, boolean isSpecial) {
		ListStruct pList = (ListStruct) set.funcall(NullStruct.INSTANCE, KeywordOld.Type, TStruct.INSTANCE);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Allocation, cons.funcall(KeywordOld.Parameter, position));
		SymbolStruct scope = ((newVariable.isSpecial() || isSpecial) ? KeywordOld.Dynamic : KeywordOld.Lexical);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Scope, scope);
		ListStruct element = ListStruct.buildProperList(newVariable, pList);
		ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.Bindings, currentEnvironment.getRest());
		((ConsStruct) bindList).setCdr(acons.funcall(newVariable, pList, bindList.getRest()));
		return currentEnvironment;
	}
    /*
     * (defun create-new-lambda-binding (curr-env new-var position special?)
     *   (acons new-var
     *     (setf (
     */

	@SuppressWarnings("unchecked")
	public static ListStruct createNewLetBinding(ListStruct currentEnvironment, SymbolStruct newVariable, IntegerStruct position, LispStruct initForm, boolean isSpecial) {
		ListStruct pList = (ListStruct) set.funcall(NullStruct.INSTANCE, KeywordOld.Type, TStruct.INSTANCE);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Allocation, cons.funcall(KeywordOld.Local, position));
		SymbolStruct scope = ((newVariable.isSpecial() || isSpecial) ? KeywordOld.Dynamic : KeywordOld.Lexical);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Scope, scope);
		pList = (ListStruct) set.funcall(pList, KeywordOld.InitForm, initForm);
		ListStruct element = ListStruct.buildProperList(newVariable, pList);
		ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.Bindings, currentEnvironment.getRest());
		((ConsStruct) bindList).setCdr(acons.funcall(newVariable, pList, bindList.getRest()));
		return currentEnvironment;
	}

	@SuppressWarnings("unchecked")
	public static ListStruct createNewFBinding(ListStruct currentEnvironment, SymbolStruct newVariable, SymbolStruct newFieldName) {
		ListStruct pList = NullStruct.INSTANCE;
		pList = (ListStruct) set.funcall(pList, KeywordOld.Name, newFieldName);
		ListStruct element = ListStruct.buildProperList(newVariable, pList);
		ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.Bindings, currentEnvironment.getRest());
		((ConsStruct) bindList).setCdr(acons.funcall(newVariable, pList, bindList.getRest()));
		return currentEnvironment;
	}

	/**
	 * Accesses the binding set in the specified environment. This method works for
	 * all environments: lambda, let, flet, and labels
	 *
	 * @param currentEnvironment the enviroment to inquire
	 * @return the binding set
	 */
	public static ListStruct getBindingSet(ListStruct currentEnvironment) {
		return (ListStruct) assoc.funcall(KeywordOld.Bindings, currentEnvironment.getRest());
	}

	/**
	 * Accesses the binding set in the specified environment. This method works for
	 * all environments: lambda, let, flet, and labels
	 *
	 * @param currentEnvironment the enviroment to inquire
	 * @return the binding set
	 */
	public static ListStruct getClosureSet(ListStruct currentEnvironment) {
		return (ListStruct) assoc.funcall(KeywordOld.Closure, currentEnvironment.getRest());
	}

	/**
	 * Access the current value and returns the binding for the specified symbol
	 * in the specified environment. This method works for
	 * all environments: lambda, let, flet, and labels
	 *
	 * @param currentEnvironment the enviroment to inquire
	 * @param variable           the symbol bound in the specified environment
	 * @return returns the binding for the specified symbol in the specified environment.
	 */
	public static ListStruct getBinding(ListStruct currentEnvironment, SymbolStruct variable) {
		ListStruct bindList = (ListStruct) assoc.funcall(variable, getBindingSet(currentEnvironment).getRest());
		return bindList.getRest();
	}

	public static SymbolStruct extractBoundName(ListStruct env, SymbolStruct fnName, boolean valueBinding) {
		// get the current environment - used for macroexpansion
		ListStruct fnBinding = EnvironmentAccessor.getBindingEnvironment(env, fnName, valueBinding);
		if (fnBinding != NullStruct.INSTANCE) {
			ListStruct aList = fnBinding.getRest(); // ((parent ...) (bindings ...)
			// use assoc to get bindings
			ListStruct aBindings = (ListStruct) assoc.funcall(KeywordOld.Bindings, aList); // (bindings (foo ...) (bar  ...) ...)
			ListStruct fooBinding = (ListStruct) assoc.funcall(fnName, aBindings.getRest()); // (foo ...)
			// see if this is a LABELS or FLET. That changes fnName
			return (SymbolStruct) GetPlist.FUNCTION.funcall(fooBinding.getRest(), KeywordOld.Name);
		} else {
			return fnName;
		}
	}

	/**
	 * Looks recursively at the environment that it is passed and all of it's ancestors until
	 * it finds one that contains the variable specified.
	 *
	 * @param currentEnvironment The environment to start looking for the symbol in.
	 * @param variable           The symbol being looked for
	 * @param valueBinding
	 * @return The list containing the binding for the given variable, or NIL if not found.
	 */
	public static ListStruct getBindingEnvironment(ListStruct currentEnvironment, SymbolStruct variable, boolean valueBinding) {
		if (currentEnvironment == NullStruct.INSTANCE) {
			return currentEnvironment;
		}
		if (getBinding(currentEnvironment, variable) != NullStruct.INSTANCE) {
			if (!(currentEnvironment.getFirst() instanceof SymbolStruct))
				System.out.println("currEnv: " + currentEnvironment.getFirst());
			SymbolStruct envType = (SymbolStruct) currentEnvironment.getFirst();
			if (((envType == LAMBDA) || (envType == MACRO) || (envType == LET)) && valueBinding) {
				return currentEnvironment;
			} else if (((envType == FLET) || (envType == LABELS)) && (!valueBinding)) {
				return currentEnvironment;
			} else if ((envType == MACROLET) && valueBinding) {
				return currentEnvironment;
			}
		}
		return getBindingEnvironment(getParent(currentEnvironment), variable, valueBinding);
	}

	public static ListStruct findClosestLambdaOrLetEnv(ListStruct currentEnvironment) {
		//TODO find out why (load-time-value) is coming here
		if (!(currentEnvironment.getFirst() instanceof SymbolStruct)) {
			return currentEnvironment;
		}
		if (currentEnvironment == NullStruct.INSTANCE) {
			return NullStruct.INSTANCE;
		}
		SymbolStruct envType = (SymbolStruct) currentEnvironment.getFirst();
		if ((envType == LAMBDA) || (envType == MACRO) || (envType == LET) || (envType == LABELS) || (envType == FLET)) {
			return currentEnvironment;
		} else {
			return findClosestLambdaOrLetEnv(getParent(currentEnvironment));
		}
	}

	/**
	 * This method adds a given symbol to the given environment.  It automatically detects correct
	 * values for scope and binding.
	 *
	 * @param currentEnvironment The environment containing the symbol table to add the symbol to.
	 * @param newSymbol          The symbol to add.
	 * @return The environment containing the change
	 */
	@SuppressWarnings("unchecked")
	public static ListStruct addSymbolToTable(ListStruct currentEnvironment, SymbolStruct newSymbol) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		/** default - may change later **/
		ListStruct pList = (ListStruct) set.funcall(NullStruct.INSTANCE, KeywordOld.Type, TStruct.INSTANCE);
		// ...(:TYPE T)
		// if this symbol is marked as special, it automatically goes to the second section
		if (!newSymbol.isSpecial()) {
			// This section deals with the lexical binding of symbols that are bound above the current env
			ListStruct bindingEnvironment = getBindingEnvironment(currentEnvironment, newSymbol, true);
			if (bindingEnvironment != NullStruct.INSTANCE) {
				// so it has a lexical binding somewhere
				if (getBinding(currentEnvironment, newSymbol) == NullStruct.INSTANCE) {
					// but not here. Have to create a reference to the existing binding env
					pList = (ListStruct) set.funcall(pList, KeywordOld.Scope, KeywordOld.Lexical);
					pList = (ListStruct) set.funcall(pList, KeywordOld.Binding, bindingEnvironment);
					// ...(:BINDING #the-binding-env# :SCOPE :LEXICAL :TYPE T)
					// so we just have to point the allocation at the binding environment
					//*** here is where we know that there is a closure involved ***
					// See if it is bound in the current lambda env. If so, just carry on
					ListStruct currentLambda = getEnclosingLambda(currentEnvironment);
					// is the binding location above the current lambda? If so, there has
					// to be a closure allocation in the bindingEnvironment
					ListStruct bindingLambda = getEnclosingLambda(bindingEnvironment);
					if (currentLambda != bindingLambda) {
						// here the binding lambda is outside the enclosing lambda
						// this calls for a closure allocation in the outer env (lambda or let)
						addClosureToBindingEnvironment(bindingEnvironment, newSymbol);
						// the (:allocation (:closure . bindingEnv)) to the table
						bindingEnvironment = (ListStruct) cons.funcall(KeywordOld.Closure, bindingEnvironment);
					}
					//***
					pList = (ListStruct) set.funcall(pList, KeywordOld.Allocation, bindingEnvironment);
					ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.SymbolTable, currentEnvironment.getRest());
					((ConsStruct) bindList).setCdr(acons.funcall(newSymbol, pList, bindList.getRest()));
				}
				// we may also be here because there is a binding right here. Nothing to do
				return currentEnvironment;
			}
		}


		// if not bound anywhere in the binding tree (free and dynamic)...
		// we know we're dealing with dynamic variables - so, is it already tagged in this environment?
		if (getSymbolInTable(currentEnvironment, newSymbol) != NullStruct.INSTANCE) {
			// yes, nothing to do
			return currentEnvironment;
		}

		// so, we at least have to add an entry in this environment
		// and possibly in an outer one
		pList = (ListStruct) set.funcall(pList, KeywordOld.Scope, KeywordOld.Dynamic);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Binding, KeywordOld.Free);
		// ...(:BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

		// if the current environment is not a lambda, we may need to add an entry to the enclosing lambda
		if (!isLambda(currentEnvironment)) {
			// if the current environment is not a lambda, find the enclosing lambda
			ListStruct enclosingLambda = getEnclosingLambda(currentEnvironment);

			// see if the symbol is already in the enclosing environment
			if (getSymbolInTable(enclosingLambda, newSymbol) != NullStruct.INSTANCE) {
				// it is, so we have to add a reference to that environment in the current env
				pList = (ListStruct) set.funcall(pList, KeywordOld.Allocation, enclosingLambda);
				ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.SymbolTable, currentEnvironment.getRest());
				((ConsStruct) bindList).setCdr(acons.funcall(newSymbol, pList, bindList.getRest()));
				return currentEnvironment;
			} else {
				// it's not in the enclosing lambda, so put in an entry
				ListStruct enclosingSymTbl = addLocalAlloc(currentEnvironment, pList);
				// add to the enclosing lambda's symbol table
				ListStruct enclosingBindList = (ListStruct) assoc.funcall(KeywordOld.SymbolTable, enclosingLambda.getRest());
				((ConsStruct) enclosingBindList).setCdr(acons.funcall(newSymbol, enclosingSymTbl, enclosingBindList.getRest()));
			}
			// Now we have the outer symbol table set up correctly
			// set the Allocation KeywordOld to what is should be for the current environment
			pList = (ListStruct) set.funcall(pList, KeywordOld.Allocation, enclosingLambda);
			// ...(:ALLOCATION #enclosing-lambda# :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		} else {
			// symbol was found in the immediate scope of the Lambda
			pList = addLocalAlloc(currentEnvironment, pList);
			// ...(:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		}
		// now we add the new symbol to the local table
		ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.SymbolTable, currentEnvironment.getRest());
		((ConsStruct) bindList).setCdr(acons.funcall(newSymbol, pList, bindList.getRest()));

		return currentEnvironment;
	}

	public static ListStruct addLocalAlloc(ListStruct currentEnvironment, ListStruct pList) {
		IntegerStruct localNumber = new IntegerStruct(BigInteger.valueOf(getNextAvailableParameterNumber(currentEnvironment)));
		ListStruct localAllocation = (ListStruct) cons.funcall(KeywordOld.Local, localNumber);
		return (ListStruct) set.funcall(pList, KeywordOld.Allocation, localAllocation);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param environment The environment that is enclosed by a lambda
	 * @return The lambda enclosing the given environment.
	 */
	public static ListStruct getEnclosingLambda(ListStruct environment) {
		// if we are looking at a lambda, return it
		if (isLambda(environment)) {
			return environment;
		}

		ListStruct parent = getParent(environment);

		// if the parent is null and there is no enclosing lambda, return NIL
		if (parent == NullStruct.INSTANCE) {
			return NullStruct.INSTANCE;
		}

		// keep looking
		return getEnclosingLambda(parent);
	}

	public static ListStruct getSymbolTable(ListStruct currentEnvironment) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		return (ListStruct) assoc.funcall(KeywordOld.SymbolTable, currentEnvironment.getRest());
	}

	public static ListStruct getSymbolInTable(ListStruct currentEnvironment, SymbolStruct variable) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		ListStruct symTable = getSymbolTable(currentEnvironment);
		ListStruct symTable2 = (ListStruct) assoc.funcall(variable, symTable.getRest());
		return symTable2.getRest();
	}

	public static ListStruct getSymbolTableEntry(ListStruct currentEnvironment, SymbolStruct variable) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		// look up the symbol in the symbol table
		ListStruct symPList = EnvironmentAccessor.getSymbolInTable(currentEnvironment, variable);
		// (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		// we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION
		ListStruct alloc = (ListStruct) get.funcall(symPList, KeywordOld.Allocation);
		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (alloc.getFirst() != KeywordOld.Local) {
			symPList = EnvironmentAccessor.getSymbolInTable(alloc.getRest(), variable);
		}
		return symPList;
	}

	public static IntegerStruct getSymbolAllocation(ListStruct currentEnvironment, SymbolStruct variable) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		// look up the symbol in the symbol table
		ListStruct symPList = getSymbolTableEntry(currentEnvironment, variable);
//        List symPList = EnvironmentAccessor.getSymbolInTable(currentEnvironment, variable);
//        // (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
//        // we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION
//        lisp.common.type.List alloc = (lisp.common.type.List)get.funcall(symPList, KeywordOld.Allocation);
//        // if the cons starts with LOCAL, we're there
//        // otherwise, we have to go to the actual env of allocation
//        if (alloc.getCar() != KeywordOld.Local) {
//            symPList = EnvironmentAccessor.getSymbolInTable((List)alloc, variable);
		ListStruct alloc = (ListStruct) get.funcall(symPList, KeywordOld.Allocation);
		if (alloc == NullStruct.INSTANCE) {
			return new IntegerStruct(BigInteger.valueOf(-1));
		} else {
			return (IntegerStruct) ((ConsStruct) alloc).getCdr();
		}
	}

	public static SymbolStruct getSymbolScope(ListStruct currentEnvironment, SymbolStruct variable) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		// look up the symbol in the symbol table
		ListStruct symPList = getSymbolTableEntry(currentEnvironment, variable);
		return (SymbolStruct) get.funcall(symPList, KeywordOld.Scope);
	}

	/**
	 * This method returns the number of the rest available parameter number. It finds the maximum
	 * value of :parameter or :local specifications in the current environment and any enclosing
	 * environment up to the enclosing lambda. This check has to be made since an allocation
	 * may happen in any environment in the stack, generating local parameter numbers that may
	 * be higher in an outer environment. This is a side-effect of the depth-first allocation
	 * algorithm.
	 * NOTE: allocation checks are made for both lexical and free variables (:bindings and :symbol-table)
	 *
	 * @param bindings   {@link ListStruct}
	 * @param currentMax int
	 * @return The int value of the rest available parameter number.
	 */
	public static int getLocalMax(ListStruct bindings, int currentMax) {
		ListStruct bindingEntry = NullStruct.INSTANCE;
		ListStruct propList = NullStruct.INSTANCE;
		ListStruct allocation = NullStruct.INSTANCE;

		int tempParameter = 0;
		// drop the :bindings at the beginning
		bindings = bindings.getRest();
		// list of bindings -> ( (..binding spec...) (...binding spec...)...)

		while (bindings != NullStruct.INSTANCE) {
			bindingEntry = (ListStruct) bindings.getFirst();
			propList = bindingEntry.getRest();
			allocation = (ListStruct) get.funcall(propList, KeywordOld.Allocation);
			// an allocation is a ref to an outer env, (:local . n) or (:parameter . n)
			Object type = allocation.getFirst();
			if ((type == KeywordOld.Local) || (type == KeywordOld.Parameter)) {
				tempParameter = ((IntegerStruct) ((ConsStruct) allocation).getCdr()).getBigInteger().intValue();
				if (tempParameter > currentMax) {
					currentMax = tempParameter;
				}
			}
			bindings = bindings.getRest();
		}
		return currentMax;

	}

	public static int getNextAvailableParameterNumber(ListStruct environment) {
		// starts with the current environment
		int max = 0;
		ListStruct bindings;

		// Loop through the envs until you hit a lambda (inclusive)
		while (true) {
			// loop up through the local env we'return looking through
			max = getLocalMax((ListStruct) assoc.funcall(KeywordOld.Bindings, environment.getRest()), max);
			// now look through thru the symbol table (free variables)
			max = getLocalMax((ListStruct) assoc.funcall(KeywordOld.SymbolTable, environment.getRest()), max);
			// see if we just handled a lambda environment
			if (isLambda(environment)) {
				// yup, all done
				break;
			}
			environment = getParent(environment);
			assert (environment != NullStruct.INSTANCE);
		}

		// return one more than the max
		return max + 1;
	}

	public static boolean isLambda(ListStruct environment) {
		return ((environment.getFirst() == LAMBDA) || (environment.getFirst() == MACRO) ||
				(environment.getFirst() == FLET) || (environment.getFirst() == LABELS));
	}

	/**
	 * This method will find the parent environment in which the given symbol is bound.
	 * If the symbol has not been added to the closure there yet, it will be added now.
	 * If it has already been added, then the number of references is incremented.  If the
	 * symbol is not bound in a parent (i.e., it is free) then nothing is done.
	 *
	 * @param currentEnvironment The environment whose parent may contain the binding for
	 *                           the given symbol.
	 * @param symbol             The symbol to add/modify the closure entry for
	 * @return The current environment with changes to the parent
	 */
	public static ListStruct addClosureToBindingEnvironment(ListStruct currentEnvironment, SymbolStruct symbol) {
		ListStruct closure;
		IntegerStruct position, references;
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		ListStruct bindingEnvironment = getBindingEnvironment(currentEnvironment, symbol, true);

		// if there is a parent binding environment
		if (bindingEnvironment != NullStruct.INSTANCE) {
			closure = getClosure(bindingEnvironment, symbol);

			// if there is not yet a closure for this variable, add it
			if (closure == NullStruct.INSTANCE) {
				// there is now one reference
				references = new IntegerStruct(BigInteger.ONE);

				// get closure from the association list
				closure = (ListStruct) assoc.funcall(KeywordOld.Closure, bindingEnvironment.getRest());

				// position will be the current size of the closure association list
				position = new IntegerStruct(BigInteger.valueOf(closure.size() - 1));
				bindingEnvironment = createNewClosure(bindingEnvironment, symbol, references, position);
			} // if the binding environment has already had a closure added to it
			// for this variable, just increment it's number of references
			else {
				// get the current number of references
				references = (IntegerStruct) get.funcall(closure, KeywordOld.References);
				// increment the number of references
				references = new IntegerStruct(references.getBigInteger().add(BigInteger.ONE));
				// set the number of references
				closure = (ListStruct) set.funcall(closure, KeywordOld.References, references);
			}
		}
		return currentEnvironment;
	}

	@SuppressWarnings("unchecked")
	public static ListStruct createNewClosure(ListStruct currentEnvironment, SymbolStruct newSymbol, IntegerStruct references, IntegerStruct position) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		ListStruct pList = (ListStruct) set.funcall(NullStruct.INSTANCE, KeywordOld.References, references);
		pList = (ListStruct) set.funcall(pList, KeywordOld.Position, position);
		ListStruct element = ListStruct.buildProperList(newSymbol, pList);
		ListStruct bindList = (ListStruct) assoc.funcall(KeywordOld.Closure, currentEnvironment.getRest());
		((ConsStruct) bindList).setCdr(acons.funcall(newSymbol, pList, bindList.getRest()));
		return currentEnvironment;
	}

//    public static List getClosureSet(List currentEnvironment) {
//        currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
//        return (List) assoc.funcall(KeywordOld.Closure, currentEnvironment.rest());
//    }

	public static ListStruct getClosure(ListStruct currentEnvironment, SymbolStruct variable) {
		currentEnvironment = findClosestLambdaOrLetEnv(currentEnvironment);
		ListStruct closure = (ListStruct) assoc.funcall(KeywordOld.Closure, currentEnvironment.getRest());
		ListStruct closure2 = (ListStruct) assoc.funcall(variable, closure.getRest());
		return closure2.getRest();
	}
}
