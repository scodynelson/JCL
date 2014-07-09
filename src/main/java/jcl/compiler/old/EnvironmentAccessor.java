package jcl.compiler.old;

import jcl.LispStruct;
import jcl.compiler.old.functions.AssocFunction;
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

public final class EnvironmentAccessor {

	public static final SymbolStruct LAMBDA = SpecialOperatorOld.LAMBDA_MARKER;
	public static final SymbolStruct MACRO = SpecialOperatorOld.MACRO_MARKER;
	public static final SymbolStruct MACROLET = SpecialOperatorOld.MACROLET;
	public static final SymbolStruct LET = SpecialOperatorOld.LET_MARKER;
	public static final SymbolStruct FLET = SpecialOperatorOld.FLET_MARKER;
	public static final SymbolStruct LABELS = SpecialOperatorOld.LABELS_MARKER;

	private EnvironmentAccessor() {
	}

	/**
	 * Creates a new reference environment for storing binding information.
	 *
	 * @param marker either LAMBDA or LET depending on type of environment
	 * @return the new reference environment
	 */
	public static ListStruct createNewEnvironment(final SymbolStruct marker) {
		final ListStruct[] assocArray;
//        if (lambdaLet.equals(FLET) || lambdaLet.equals(LABELS)) {
//            assocArray = new ListStruct[]{
//                        ListStruct.buildProperList(KeywordOld.Parent),
//                        ListStruct.buildProperList(KeywordOld.Bindings)};
//        } else {
		assocArray = new ListStruct[]{
				ListStruct.buildProperList(KeywordOld.Parent),
				ListStruct.buildProperList(KeywordOld.Bindings),
				ListStruct.buildProperList(KeywordOld.SymbolTable),
				ListStruct.buildProperList(KeywordOld.Closure)};
//        }
		// ((:parent)(:bindings)(:symbol-table)(:closure))

		ListStruct newList = ListStruct.buildProperList(assocArray);
		// now we have place for static initializers for LOAD-TIME-VALUE
		if (marker.equals(LAMBDA) || marker.equals(MACRO) || marker.equals(FLET) || marker.equals(LABELS)) {
			newList = new ConsStruct(ListStruct.buildProperList(KeywordOld.LoadTimeValue), newList);
			// ((load-time-value) (:parent)(:bindings)(:symbol-table)(:closure))
		}
		newList = new ConsStruct(marker, newList);
		// ('lambdaLet' (:load-time-value) (:parent)(:bindings)(:symbol-table)(:closure))

		// set parent to NIL as the default value
		newList = createParent(newList, NullStruct.INSTANCE); // NULL/NIL parent
		// ('lambdaLet' (:load-time-value) (:parent)(:bindings)(:symbol-table)(:closure))
		return newList;
	}

	/*
	 * (defun create-new-environment (marker)
     *   (let ((new-list '((:parent nil) (:bindings) (:symbol-table) (:closure))))
     *     (cons marker
     *           (case marker
     *             ((lambda macro flet labels)
     *              (cons '(:load-time-value) new-list)
     *             (t new-list)))))
     */

	/**
	 * Creates a parent reference for the current reference environment.
	 *
	 * @param currentEnvironment the reference environment to which the parent is to be added
	 * @param parent             the lambda reference that is the parent of the current reference environment
	 * @return the new reference environment
	 */
	public static ListStruct createParent(final ListStruct currentEnvironment, final ListStruct parent) {
		// ('lambdaLet' (:parent ...) ...)
		final ListStruct assocList = AssocFunction.funcall(KeywordOld.Parent, currentEnvironment.getRest());
		// (:parent ...)

		((ConsStruct) assocList).setCdr(parent);
		// (:parent 'parent')
		return currentEnvironment;
	}

	/*
	 * (defun create-parent (curr-env parent)
	 *   (setf (rest (assoc :parent (rest curr-env))) parent)
	 *   curr-env)
	 */

	/**
	 * Creates an environment with a NIL parent. This is equivalent to the global
	 * environment. This is used at the beginning of semantic analysis and anywhere a
	 * global environment is needed, for example, load-time-value.
	 *
	 * @return global environment ListStruct
	 */
	public static ListStruct createGlobalEnvironment() {
		final ListStruct newEnvironment = createNewEnvironment(LAMBDA);
		return createParent(newEnvironment, NullStruct.INSTANCE); // NULL/NIL parent
	}

    /*
     * (defun create-global-environment ()
     *   (create-new-environment 'lambda)
     *   (create-parent))
     */

	/**
	 * Returns the current reference environment's parent environment.
	 *
	 * @param currentEnvironment the reference environment from which the parent is to be acquired
	 * @return the parent reference environment
	 */
	public static ListStruct getParent(final ListStruct currentEnvironment) {
		final ListStruct assocList = AssocFunction.funcall(KeywordOld.Parent, currentEnvironment.getRest());
		// (:parent ...)
		return (ListStruct) assocList.getRest().getFirst(); // AKA. (second assocList)
	}

	/*
	 * (defun get-parent (curr-env)
	 *   (second (assoc :parent (rest curr-env)))
	 */

	public static ListStruct createNewLambdaBinding(final ListStruct currentEnvironment, final SymbolStruct newVariable,
	                                                final IntegerStruct position, final boolean isSpecial) {

		ListStruct pList = new ConsStruct(KeywordOld.Type, ListStruct.buildProperList(TStruct.INSTANCE));
		pList = SetPlist.funcall(pList, KeywordOld.Allocation, new ConsStruct(KeywordOld.Parameter, position));
		final SymbolStruct scope = (newVariable.isSpecial() || isSpecial) ? KeywordOld.Dynamic : KeywordOld.Lexical;
		pList = SetPlist.funcall(pList, KeywordOld.Scope, scope);
		final ListStruct element = ListStruct.buildProperList(newVariable, pList);
		final ListStruct bindList = getBindingSet(currentEnvironment);
		((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
		return currentEnvironment;
	}

	/*
	EXAMPLE:

	curr-env == '(marker ((:bindings ((x (:type (t) :allocation (:parameter 0) :scope :dynamic))))))
	new-var == a
	position == 1
	special-p == nil

	result =>
	curr-env == '(marker ((:bindings ((a (:type (t) :allocation (:parameter 1) :scope :lexical))
									  (x (:type (t) :allocation (:parameter 0) :scope :dynamic))))))
	 */

	/*
	 * (defun create-new-lambda-binding (curr-env new-var position special-p)
	 *   (setf plist (list :type '(t)
	 *   				   :allocation '(:parameter position)
	 *   				   :scope (if special-p :dynamic :lexical)))
	 *   (setf (rest (assoc :bindings (rest curr-env)))
	 *   	   (cons (cons new-var plist)
	 *         		 (rest (assoc :bindings (rest curr-env)))))
	 *   curr-env)
     */

	public static ListStruct createNewLetBinding(final ListStruct currentEnvironment, final SymbolStruct newVariable,
	                                             final IntegerStruct position, final LispStruct initForm, final boolean isSpecial) {

		ListStruct pList = new ConsStruct(KeywordOld.Type, ListStruct.buildProperList(TStruct.INSTANCE));
		pList = SetPlist.funcall(pList, KeywordOld.Allocation, new ConsStruct(KeywordOld.Local, position));
		final SymbolStruct scope = (newVariable.isSpecial() || isSpecial) ? KeywordOld.Dynamic : KeywordOld.Lexical;
		pList = SetPlist.funcall(pList, KeywordOld.Scope, scope);
		pList = SetPlist.funcall(pList, KeywordOld.InitForm, initForm);
		final ListStruct element = ListStruct.buildProperList(newVariable, pList);
		final ListStruct bindList = getBindingSet(currentEnvironment);
		((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
		return currentEnvironment;
	}

	/*
	EXAMPLE:

	curr-env == '(marker ((:bindings ((x (:type (t) :allocation (:parameter 0) :scope :dynamic :init-form "Hello"))))))
	new-var == a
	position == 1
	init-form == "World"
	special-p == nil

	result =>
	curr-env == '(marker ((:bindings ((a (:type (t) :allocation (:parameter 1) :scope :lexical :init-form "World"))
									  (x (:type (t) :allocation (:parameter 0) :scope :dynamic :init-form "Hello"))))))
	 */

	/*
	 * (defun create-new-let-binding (curr-env new-var position init-form special-p)
	 *   (setf plist (list :type '(t)
	 *   				   :allocation '(:local position)
	 *   				   :scope (if special-p :dynamic :lexical)
	 *   				   :init-form init-form))
	 *   (setf (rest (assoc :bindings (rest curr-env)))
	 *   	   (cons (cons new-var plist)
	 *         		 (rest (assoc :bindings (rest curr-env)))))
	 *   curr-env)
     */

	public static ListStruct createNewFBinding(final ListStruct currentEnvironment, final SymbolStruct newVariable,
	                                           final SymbolStruct newFieldName) {

		final ListStruct pList = new ConsStruct(KeywordOld.Name, ListStruct.buildProperList(newFieldName));
		final ListStruct element = ListStruct.buildProperList(newVariable, pList);
		final ListStruct bindList = getBindingSet(currentEnvironment);
		((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
		return currentEnvironment;
	}

	/*
	EXAMPLE:

	curr-env == '(marker ((:bindings ((x (:name ("HELLO")))))))
	new-var == a
	new-field-name == "WORLD"

	result =>
	curr-env == '(marker ((:bindings ((z (:name ("WORLD")))
									  (x (:name ("HELLO")))))))
	 */

	/*
	 * (defun create-new-f-binding (curr-env new-var new-field-name)
	 *   (setf plist (list :name '(new-field-name)))
	 *   (setf (rest (assoc :bindings (rest curr-env)))
	 *   	   (cons (cons new-var plist)
	 *         		 (rest (assoc :bindings (rest curr-env)))))
	 *   curr-env)
     */

	/**
	 * Accesses the binding set in the specified environment. This method works for
	 * all environments: lambda, let, flet, and labels
	 *
	 * @param currentEnvironment the enviroment to inquire
	 * @return the binding set
	 */
	public static ListStruct getBindingSet(final ListStruct currentEnvironment) {
		return AssocFunction.funcall(KeywordOld.Bindings, currentEnvironment.getRest());
	}

	/**
	 * Accesses the binding set in the specified environment. This method works for
	 * all environments: lambda, let, flet, and labels
	 *
	 * @param currentEnvironment the enviroment to inquire
	 * @return the binding set
	 */
	public static ListStruct getClosureSet(final ListStruct currentEnvironment) {
		return AssocFunction.funcall(KeywordOld.Closure, currentEnvironment.getRest());
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
	public static ListStruct getBinding(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct bindList = AssocFunction.funcall(variable, getBindingSet(currentEnvironment).getRest());
		return bindList.getRest();
	}

	public static SymbolStruct extractBoundName(final ListStruct env, final SymbolStruct fnName, final boolean valueBinding) {

		// get the current environment - used for macroexpansion
		final ListStruct fnBinding = getBindingEnvironment(env, fnName, valueBinding);
		if (fnBinding.equals(NullStruct.INSTANCE)) {
			return fnName;
		}

		final ListStruct aList = fnBinding.getRest(); // ((parent ...) (bindings ...)
		// use assoc to get bindings
		final ListStruct aBindings = AssocFunction.funcall(KeywordOld.Bindings, aList); // (bindings (foo ...) (bar  ...) ...)
		final ListStruct fooBinding = AssocFunction.funcall(fnName, aBindings.getRest()); // (foo ...)
		// see if this is a LABELS or FLET. That changes fnName
		return (SymbolStruct) GetPlist.funcall(fooBinding.getRest(), KeywordOld.Name);
	}

	/**
	 * Looks recursively at the environment that it is passed and all of it's ancestors until
	 * it finds one that contains the variable specified.
	 *
	 * @param currentEnvironment The environment to start looking for the symbol in.
	 * @param variable           The symbol being looked for
	 * @param valueBinding       if the value is binding
	 * @return The list containing the binding for the given variable, or NIL if not found.
	 */
	public static ListStruct getBindingEnvironment(final ListStruct currentEnvironment, final SymbolStruct variable,
	                                               final boolean valueBinding) {

		if (currentEnvironment.equals(NullStruct.INSTANCE)) {
			return currentEnvironment;
		}

		if (!getBinding(currentEnvironment, variable).equals(NullStruct.INSTANCE)) {
			if (!(currentEnvironment.getFirst() instanceof SymbolStruct)) {
				System.out.println("currEnv: " + currentEnvironment.getFirst());
			}

			final SymbolStruct envType = (SymbolStruct) currentEnvironment.getFirst();
			if ((envType.equals(LAMBDA) || envType.equals(MACRO) || envType.equals(LET)) && valueBinding) {
				return currentEnvironment;
			}

			if ((envType.equals(FLET) || envType.equals(LABELS)) && !valueBinding) {
				return currentEnvironment;
			}

			if (envType.equals(MACROLET) && valueBinding) {
				return currentEnvironment;
			}
		}

		return getBindingEnvironment(getParent(currentEnvironment), variable, valueBinding);
	}

	public static ListStruct findClosestLambdaOrLetEnv(final ListStruct currentEnvironment) {

		//TODO find out why (load-time-value) is coming here
		if (!(currentEnvironment.getFirst() instanceof SymbolStruct)) {
			return currentEnvironment;
		}

		if (currentEnvironment.equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		final SymbolStruct envType = (SymbolStruct) currentEnvironment.getFirst();
		if (envType.equals(LAMBDA) || envType.equals(MACRO) || envType.equals(LET) || envType.equals(LABELS) || envType.equals(FLET)) {
			return currentEnvironment;
		}

		return findClosestLambdaOrLetEnv(getParent(currentEnvironment));
	}

	/**
	 * This method adds a given symbol to the given environment.  It automatically detects correct
	 * values for scope and binding.
	 *
	 * @param currentEnvironment The environment containing the symbol table to add the symbol to.
	 * @param newSymbol          The symbol to add.
	 * @return The environment containing the change
	 */
	public static ListStruct addSymbolToTable(final ListStruct currentEnvironment, final SymbolStruct newSymbol) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		/* default - may change later */
		ListStruct pList = new ConsStruct(KeywordOld.Type, ListStruct.buildProperList(TStruct.INSTANCE));

		// ...(:TYPE T)
		// if this symbol is marked as special, it automatically goes to the second section
		if (!newSymbol.isSpecial()) {

			// This section deals with the lexical binding of symbols that are bound above the current env
			ListStruct bindingEnvironment = getBindingEnvironment(currentEnvironmentInner, newSymbol, true);
			if (!bindingEnvironment.equals(NullStruct.INSTANCE)) {

				// so it has a lexical binding somewhere
				if (getBinding(currentEnvironmentInner, newSymbol).equals(NullStruct.INSTANCE)) {

					// but not here. Have to create a reference to the existing binding env
					pList = SetPlist.funcall(pList, KeywordOld.Scope, KeywordOld.Lexical);
					pList = SetPlist.funcall(pList, KeywordOld.Binding, bindingEnvironment);

					// ...(:BINDING #the-binding-env# :SCOPE :LEXICAL :TYPE T)
					// so we just have to point the allocation at the binding environment
					//*** here is where we know that there is a closure involved ***
					// See if it is bound in the current lambda env. If so, just carry on
					final ListStruct currentLambda = getEnclosingLambda(currentEnvironmentInner);

					// is the binding location above the current lambda? If so, there has
					// to be a closure allocation in the bindingEnvironment
					final ListStruct bindingLambda = getEnclosingLambda(bindingEnvironment);
					if (!currentLambda.equals(bindingLambda)) {

						// here the binding lambda is outside the enclosing lambda
						// this calls for a closure allocation in the outer env (lambda or let)
						addClosureToBindingEnvironment(bindingEnvironment, newSymbol);

						// the (:allocation (:closure . bindingEnv)) to the table
						bindingEnvironment = new ConsStruct(KeywordOld.Closure, bindingEnvironment);
					}
					//***
					pList = SetPlist.funcall(pList, KeywordOld.Allocation, bindingEnvironment);
					final ListStruct element = ListStruct.buildProperList(newSymbol, pList);
					final ListStruct bindList = AssocFunction.funcall(KeywordOld.SymbolTable, currentEnvironmentInner.getRest());
					((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
				}

				// we may also be here because there is a binding right here. Nothing to do
				return currentEnvironmentInner;
			}
		}


		// if not bound anywhere in the binding tree (free and dynamic)...
		// we know we're dealing with dynamic variables - so, is it already tagged in this environment?
		if (!getSymbolInTable(currentEnvironmentInner, newSymbol).equals(NullStruct.INSTANCE)) {
			// yes, nothing to do
			return currentEnvironmentInner;
		}

		// so, we at least have to add an entry in this environment
		// and possibly in an outer one
		pList = SetPlist.funcall(pList, KeywordOld.Scope, KeywordOld.Dynamic);
		pList = SetPlist.funcall(pList, KeywordOld.Binding, KeywordOld.Free);

		// ...(:BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

		// if the current environment is not a lambda, we may need to add an entry to the enclosing lambda
		if (isLambda(currentEnvironmentInner)) {

			// symbol was found in the immediate scope of the Lambda
			pList = addLocalAlloc(currentEnvironmentInner, pList);

			// ...(:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		} else {

			// if the current environment is not a lambda, find the enclosing lambda
			final ListStruct enclosingLambda = getEnclosingLambda(currentEnvironmentInner);

			// see if the symbol is already in the enclosing environment
			if (getSymbolInTable(enclosingLambda, newSymbol).equals(NullStruct.INSTANCE)) {

				// it's not in the enclosing lambda, so put in an entry
				final ListStruct enclosingSymTbl = addLocalAlloc(currentEnvironmentInner, pList);

				// add to the enclosing lambda's symbol table
				final ListStruct element = ListStruct.buildProperList(newSymbol, enclosingSymTbl);
				final ListStruct enclosingBindList = AssocFunction.funcall(KeywordOld.SymbolTable, enclosingLambda.getRest());
				((ConsStruct) enclosingBindList).setCdr(new ConsStruct(element, enclosingBindList.getRest()));
			} else {
				// it is, so we have to add a reference to that environment in the current env
				pList = SetPlist.funcall(pList, KeywordOld.Allocation, enclosingLambda);
				final ListStruct element = ListStruct.buildProperList(newSymbol, pList);
				final ListStruct bindList = AssocFunction.funcall(KeywordOld.SymbolTable, currentEnvironmentInner.getRest());
				((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
				return currentEnvironmentInner;
			}

			// Now we have the outer symbol table set up correctly
			// set the Allocation KeywordOld to what is should be for the current environment
			pList = SetPlist.funcall(pList, KeywordOld.Allocation, enclosingLambda);

			// ...(:ALLOCATION #enclosing-lambda# :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		}

		// now we add the new symbol to the local table
		final ListStruct element = ListStruct.buildProperList(newSymbol, pList);
		final ListStruct bindList = AssocFunction.funcall(KeywordOld.SymbolTable, currentEnvironmentInner.getRest());
		((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));

		return currentEnvironmentInner;
	}

	public static ListStruct addLocalAlloc(final ListStruct currentEnvironment, final ListStruct pList) {
		final IntegerStruct localNumber = new IntegerStruct(BigInteger.valueOf(getNextAvailableParameterNumber(currentEnvironment)));
		final ListStruct localAllocation = new ConsStruct(KeywordOld.Local, localNumber);
		return SetPlist.funcall(pList, KeywordOld.Allocation, localAllocation);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param environment The environment that is enclosed by a lambda
	 * @return The lambda enclosing the given environment.
	 */
	public static ListStruct getEnclosingLambda(final ListStruct environment) {
		// if we are looking at a lambda, return it
		if (isLambda(environment)) {
			return environment;
		}

		final ListStruct parent = getParent(environment);

		// if the parent is null and there is no enclosing lambda, return NIL
		if (parent.equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		// keep looking
		return getEnclosingLambda(parent);
	}

	public static ListStruct getSymbolTable(final ListStruct currentEnvironment) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);
		return AssocFunction.funcall(KeywordOld.SymbolTable, currentEnvironmentInner.getRest());
	}

	public static ListStruct getSymbolInTable(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		final ListStruct symTable = getSymbolTable(currentEnvironmentInner);
		final ListStruct symTable2 = AssocFunction.funcall(variable, symTable.getRest());
		return symTable2.getRest();
	}

	public static ListStruct getSymbolTableEntry(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		ListStruct symPList = getSymbolInTable(currentEnvironmentInner, variable);

		// (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		// we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION
		final ListStruct alloc = (ListStruct) GetPlist.funcall(symPList, KeywordOld.Allocation);

		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (!alloc.getFirst().equals(KeywordOld.Local)) {
			symPList = getSymbolInTable(alloc.getRest(), variable);
		}

		return symPList;
	}

	public static IntegerStruct getSymbolAllocation(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		final ListStruct symPList = getSymbolTableEntry(currentEnvironmentInner, variable);
//        List symPList = getSymbolInTable(currentEnvironmentInner, variable);

//        // (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
//        // we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION

		final ListStruct alloc = (ListStruct) GetPlist.funcall(symPList, KeywordOld.Allocation);

//        // if the cons starts with LOCAL, we're there
//        // otherwise, we have to go to the actual env of allocation
//        if (alloc.getFirst() != KeywordOld.Local) {
//            symPList = getSymbolInTable(alloc, variable);
		if (alloc.equals(NullStruct.INSTANCE)) {
			return new IntegerStruct(BigInteger.valueOf(-1));
		} else {
			return (IntegerStruct) ((ConsStruct) alloc).getCdr();
		}
	}

	public static SymbolStruct getSymbolScope(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		final ListStruct symPList = getSymbolTableEntry(currentEnvironmentInner, variable);
		return (SymbolStruct) GetPlist.funcall(symPList, KeywordOld.Scope);
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
	public static int getLocalMax(final ListStruct bindings, int currentMax) {

		// drop the :bindings at the beginning
		ListStruct bindingsInner = bindings.getRest();
		// list of bindings -> ( (..binding spec...) (...binding spec...)...)

		int currentMaxInner = currentMax;

		while (!bindingsInner.equals(NullStruct.INSTANCE)) {
			final ListStruct bindingEntry = (ListStruct) bindingsInner.getFirst();
			final ListStruct propList = bindingEntry.getRest();
			final ListStruct allocation = (ListStruct) GetPlist.funcall(propList, KeywordOld.Allocation);

			// an allocation is a ref to an outer env, (:local . n) or (:parameter . n)
			final Object type = allocation.getFirst();
			if (type.equals(KeywordOld.Local) || type.equals(KeywordOld.Parameter)) {

				final int tempParameter = ((IntegerStruct) ((ConsStruct) allocation).getCdr()).getBigInteger().intValue();
				if (tempParameter > currentMaxInner) {
					currentMaxInner = tempParameter;
				}
			}

			bindingsInner = bindingsInner.getRest();
		}

		return currentMaxInner;
	}

	public static int getNextAvailableParameterNumber(final ListStruct environment) {
		ListStruct environmentInner = environment;

		// starts with the current environment
		int max = 0;
		ListStruct bindings;

		// Loop through the envs until you hit a lambda (inclusive)
		while (true) {

			// loop up through the local env we'return looking through
			max = getLocalMax(AssocFunction.funcall(KeywordOld.Bindings, environmentInner.getRest()), max);

			// now look through thru the symbol table (free variables)
			max = getLocalMax(AssocFunction.funcall(KeywordOld.SymbolTable, environmentInner.getRest()), max);

			// see if we just handled a lambda environment
			if (isLambda(environmentInner)) {
				// yup, all done
				break;
			}

			environmentInner = getParent(environmentInner);
			assert !environmentInner.equals(NullStruct.INSTANCE);
		}

		// return one more than the max
		return max + 1;
	}

	public static boolean isLambda(final ListStruct environment) {
		return environment.getFirst().equals(LAMBDA) || environment.getFirst().equals(MACRO) || environment.getFirst().equals(FLET) || environment.getFirst().equals(LABELS);
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
	public static ListStruct addClosureToBindingEnvironment(final ListStruct currentEnvironment, final SymbolStruct symbol) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);
		ListStruct bindingEnvironment = getBindingEnvironment(currentEnvironmentInner, symbol, true);

		// if there is a parent binding environment
		if (!bindingEnvironment.equals(NullStruct.INSTANCE)) {
			ListStruct closure = getClosure(bindingEnvironment, symbol);

			// if there is not yet a closure for this variable, add it
			IntegerStruct references;
			if (closure.equals(NullStruct.INSTANCE)) {

				// there is now one reference
				references = new IntegerStruct(BigInteger.ONE);

				// get closure from the association list
				closure = AssocFunction.funcall(KeywordOld.Closure, bindingEnvironment.getRest());

				// position will be the current size of the closure association list
				final IntegerStruct position = new IntegerStruct(BigInteger.valueOf(closure.size() - 1));
				bindingEnvironment = createNewClosure(bindingEnvironment, symbol, references, position);
			} // if the binding environment has already had a closure added to it

			// for this variable, just increment it's number of references
			else {
				// get the current number of references
				references = (IntegerStruct) GetPlist.funcall(closure, KeywordOld.References);

				// increment the number of references
				references = new IntegerStruct(references.getBigInteger().add(BigInteger.ONE));

				// set the number of references
				closure = SetPlist.funcall(closure, KeywordOld.References, references);
			}
		}
		return currentEnvironmentInner;
	}

	public static ListStruct createNewClosure(final ListStruct currentEnvironment, final SymbolStruct newSymbol,
	                                          final IntegerStruct references, final IntegerStruct position) {

		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		ListStruct pList = SetPlist.funcall(NullStruct.INSTANCE, KeywordOld.References, references);
		pList = SetPlist.funcall(pList, KeywordOld.Position, position);
		final ListStruct element = ListStruct.buildProperList(newSymbol, pList);
		final ListStruct bindList = AssocFunction.funcall(KeywordOld.Closure, currentEnvironmentInner.getRest());
		((ConsStruct) bindList).setCdr(new ConsStruct(element, bindList.getRest()));
		return currentEnvironmentInner;
	}

//    public static ListStruct getClosureSet(final ListStruct currentEnvironment) {
//        final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);
//        return AssocFunction.funcall(KeywordOld.Closure, currentEnvironmentInner.rest());
//    }

	public static ListStruct getClosure(final ListStruct currentEnvironment, final SymbolStruct variable) {
		final ListStruct currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		final ListStruct closure = AssocFunction.funcall(KeywordOld.Closure, currentEnvironmentInner.getRest());
		final ListStruct closure2 = AssocFunction.funcall(variable, closure.getRest());
		return closure2.getRest();
	}
}
