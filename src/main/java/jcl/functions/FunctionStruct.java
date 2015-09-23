package jcl.functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.FunctionType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = 7356724806391677112L;

	protected OrdinaryLambdaListBindings lambdaListBindings;

	protected Closure closure;

	protected static final LispStruct INIT_FORM_PLACEHOLDER = new LispStruct() {

		private static final long serialVersionUID = 8747573493160199571L;

		@Override
		public LispType getType() {
			return null;
		}
	};

	/**
	 * Protected constructor.
	 */
	protected FunctionStruct() {
		this(null, FunctionType.INSTANCE, null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param closure
	 * 		instance function closure
	 */
	protected FunctionStruct(final String documentation, final Closure closure) {
		this(documentation, FunctionType.INSTANCE, null, null);
		this.closure = closure;
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param lambdaListBindings
	 * 		lambda-list bindings for the function
	 */
	protected FunctionStruct(final String documentation, final OrdinaryLambdaListBindings lambdaListBindings) {
		this(documentation, FunctionType.INSTANCE, null, null);
		this.lambdaListBindings = lambdaListBindings;
	}

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, FunctionType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final String documentation,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(documentation, FunctionType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the function object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final LispType type,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, type, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param type
	 * 		the type of the function object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final String documentation, final LispType type,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
	}

	/**
	 * This is the application method for any function structure.
	 *
	 * @param lispStructs
	 * 		the function arguments
	 *
	 * @return the result object
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	public LispStruct apply(final LispStruct... lispStructs) {
		final Map<SymbolStruct<?>, LispStruct> closureSymbolsToBind = getClosureSymbolBindings();
		for (final Map.Entry<SymbolStruct<?>, LispStruct> closureSymbolToBind : closureSymbolsToBind.entrySet()) {
			final SymbolStruct symbol = closureSymbolToBind.getKey();
			LispStruct value = closureSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final Map<SymbolStruct<?>, FunctionStruct> closureFunctionsToBind = getClosureFunctionBindings();
		for (final Map.Entry<SymbolStruct<?>, FunctionStruct> closureFunctionToBind : closureFunctionsToBind.entrySet()) {
			final SymbolStruct<?> symbol = closureFunctionToBind.getKey();
			final FunctionStruct function = closureFunctionToBind.getValue();
			symbol.bindFunction(function);
		}

		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(lispStructs);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStruct symbol = parameterSymbolToBind.getParameterSymbol();
			LispStruct value = parameterSymbolToBind.getParameterValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			} else if (INIT_FORM_PLACEHOLDER.equals(value)) {
				value = getInitForm(closure, symbol);
			}
			final boolean isSpecial = parameterSymbolToBind.isSpecial();
			if (isSpecial) {
				symbol.bindDynamicValue(value);
			} else {
				symbol.bindLexicalValue(value);
			}
		}

		final LispStruct result;
		try {
			result = internalApply(closure);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final FunctionParameterBinding parameterSymbolToUnbind : parameterSymbolsToBind) {
				final SymbolStruct<?> parameterSymbol = parameterSymbolToUnbind.getParameterSymbol();
				final boolean isSpecial = parameterSymbolToUnbind.isSpecial();
				if (isSpecial) {
					parameterSymbol.unbindDynamicValue();
				} else {
					parameterSymbol.unbindLexicalValue();
				}
			}
			for (final SymbolStruct<?> closureFunctionToUnbind : closureFunctionsToBind.keySet()) {
				closureFunctionToUnbind.unbindFunction();
			}
			for (final SymbolStruct<?> closureSymbolToUnbind : closureSymbolsToBind.keySet()) {
				closureSymbolToUnbind.unbindLexicalValue();
			}
		}
		return result;
	}

	protected LispStruct internalApply(final Closure currentClosure) {
		// TODO: replace with abstract method when finished refactoring Lambda Generation
		return null;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public void setLambdaListBindings(final OrdinaryLambdaListBindings lambdaListBindings) {
		this.lambdaListBindings = lambdaListBindings;
	}

	public Closure getClosure() {
		return closure;
	}

	public void setClosure(final Closure closure) {
		this.closure = closure;
	}

	public Map<SymbolStruct<?>, LispStruct> getClosureSymbolBindings() {
		if (closure == null) {
			return Collections.emptyMap();
		}
		return closure.getSymbolBindings();
	}

	public Map<SymbolStruct<?>, FunctionStruct> getClosureFunctionBindings() {
		if (closure == null) {
			return Collections.emptyMap();
		}
		return closure.getFunctionBindings();
	}

	protected List<FunctionParameterBinding> getFunctionBindings(final LispStruct[] lispStructs) {
		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();
		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();

		final List<FunctionParameterBinding> functionParametersToBind = new ArrayList<>();

		final List<LispStruct> functionArguments = Arrays.asList(lispStructs);
		final int numberOfArguments = functionArguments.size();
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		final String functionClassName = getClass().getSimpleName();
		final int numberOfRequired = requiredBindings.size();
		for (final RequiredBinding requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final SymbolStruct<?> requiredSymbol = requiredBinding.getSymbolStruct();
			final LispStruct requiredInitForm = functionArgumentsIterator.next();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(requiredSymbol, requiredInitForm, requiredBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		for (final OptionalBinding optionalBinding : optionalBindings) {
			final LispStruct optionalInitForm;
			final LispStruct suppliedPInitForm;

			if (functionArgumentsIterator.hasNext()) {
				optionalInitForm = functionArgumentsIterator.next();
				suppliedPInitForm = TStruct.INSTANCE;
			} else {
				optionalInitForm = INIT_FORM_PLACEHOLDER;
				suppliedPInitForm = NILStruct.INSTANCE;
			}
			final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		final int numberOfKeys = keyBindings.size();
		final Map<SymbolStruct<?>, KeyBinding> keysToBindings = new HashMap<>();
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> keyName = keyBinding.getKeyName();
			keysToBindings.put(keyName, keyBinding);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keysToBindings'
		final Set<SymbolStruct<?>> keys = new HashSet<>(keysToBindings.keySet());

		final List<LispStruct> restList = new ArrayList<>();

		while (functionArgumentsIterator.hasNext()) {
			final LispStruct nextArgument = functionArgumentsIterator.next();
			restList.add(nextArgument);
		}

		final List<SymbolStruct<?>> otherKeys = new ArrayList<>();

		final Map<SymbolStruct<?>, FunctionParameterBinding> keywordFunctionParametersToBind = new LinkedHashMap<>(keyBindings.size());
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, INIT_FORM_PLACEHOLDER, keyBinding.isSpecial());
			keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.isSpecial());
			keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof SymbolStruct) {
				final SymbolStruct<?> keywordArgument = (SymbolStruct) nextArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyBinding keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
							if (!keyInitForm.equals(NullStruct.INSTANCE) && !keyInitForm.equals(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
						final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();
						final LispStruct suppliedPInitForm = TStruct.INSTANCE;

						functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
						keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.equals(NullStruct.INSTANCE) && !allowOtherKeysValue.equals(NILStruct.INSTANCE)) {
						allowOtherKeys = true;
					}
				} else {
					if (iterator.hasNext()) {
						// Consume the next argument
						iterator.next();
					}
					// Check in case the key was supplied twice.
					if (!keys.contains(keywordArgument)) {
						otherKeys.add(keywordArgument);
					}
				}
			} else if (!keysToBindings.isEmpty()) {
				throw new ProgramErrorException("Expected Keyword argument for call to '" + functionClassName + " was: " + nextArgument);
			} else if (restBinding == null) {
				final int numberOfOptionals = optionalBindings.size();
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				throw new ProgramErrorException("Too many arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at most " + maxNumberProvided + " accepted.");
			}
		}

		if (!allowOtherKeys && !otherKeys.isEmpty() && !keys.isEmpty()) {
			throw new ProgramErrorException("Keyword arguments not found in '" + functionClassName + "' function definition: " + otherKeys);
		}

		if (restBinding != null) {
			final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
			final LispStruct restListStruct = ListStruct.buildProperList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		functionParametersToBind.addAll(keywordFunctionParametersToBind.values());

		for (final AuxBinding auxBinding : auxBindings) {
			final SymbolStruct<?> auxSymbol = auxBinding.getSymbolStruct();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(auxSymbol, INIT_FORM_PLACEHOLDER, auxBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		return functionParametersToBind;
	}

	protected LispStruct getInitForm(final Closure currentClosure, final SymbolStruct<?> parameter) {
		// TODO: replace with abstract method when finished refactoring Lambda Generation
		return null;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(lambdaListBindings)
		                            .append(closure)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final FunctionStruct rhs = (FunctionStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(lambdaListBindings, rhs.lambdaListBindings)
		                          .append(closure, rhs.closure)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(lambdaListBindings)
		                                                                .append(closure)
		                                                                .toString();
	}
}
