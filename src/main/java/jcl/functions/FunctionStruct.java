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
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordStruct;
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
	protected FunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
	public abstract LispStruct apply(LispStruct... lispStructs);

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

	protected Map<SymbolStruct<?>, LispStruct> getFunctionBindings(final LispStruct[] lispStructs) {
		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();

		final Map<SymbolStruct<?>, LispStruct> symbolsToBind = new LinkedHashMap<>();

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
			symbolsToBind.put(requiredSymbol, requiredInitForm);
		}

		for (final OptionalBinding optionalBinding : optionalBindings) {
			final LispStruct suppliedPInitForm;

			if (functionArgumentsIterator.hasNext()) {
				final LispStruct optionalInitForm = functionArgumentsIterator.next();
				suppliedPInitForm = TStruct.INSTANCE;

				final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();
				symbolsToBind.put(optionalSymbol, optionalInitForm);
			} else {
				suppliedPInitForm = NILStruct.INSTANCE;
			}

			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();
			symbolsToBind.put(suppliedPSymbol, suppliedPInitForm);
		}

		final int numberOfKeys = keyBindings.size();
		final Map<KeywordStruct, KeyBinding> keysToBindings = new HashMap<>();
		for (final KeyBinding keyBinding : keyBindings) {
			final KeywordStruct key = keyBinding.getKeyName();
			keysToBindings.put(key, keyBinding);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keysToBindings'
		final Set<KeywordStruct> keys = new HashSet<>(keysToBindings.keySet());

		final List<LispStruct> restList = new ArrayList<>();

		while (functionArgumentsIterator.hasNext()) {
			final LispStruct nextArgument = functionArgumentsIterator.next();
			restList.add(nextArgument);
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			LispStruct possiblyKeywordNexArgument = nextArgument;
			if (nextArgument instanceof SymbolStruct) {
				final SymbolStruct<?> symbolArgument = (SymbolStruct) nextArgument;
				possiblyKeywordNexArgument = getKeywordStruct(symbolArgument.getName());
			}

			if (nextArgument instanceof KeywordStruct) {
				if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(possiblyKeywordNexArgument)) {
					final LispStruct allowOtherKeysValue = functionArgumentsIterator.next();
					if (!allowOtherKeysValue.equals(NullStruct.INSTANCE) && !allowOtherKeysValue.equals(NILStruct.INSTANCE)) {
						allowOtherKeys = true;
					}
					continue;
				}

				final KeywordStruct keywordArgument = (KeywordStruct) possiblyKeywordNexArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyBinding keyBinding = keysToBindings.remove(keywordArgument);

					final LispStruct suppliedPInitForm;

					if (iterator.hasNext()) {
						final LispStruct keyInitForm = iterator.next();
						suppliedPInitForm = TStruct.INSTANCE;

						final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
						symbolsToBind.put(keySymbol, keyInitForm);
					} else {
						suppliedPInitForm = NILStruct.INSTANCE;
					}

					final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
					final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();
					symbolsToBind.put(suppliedPSymbol, suppliedPInitForm);
				} else if (keys.contains(keywordArgument) || allowOtherKeys || keysToBindings.isEmpty()) {
					if (iterator.hasNext()) {
						// Consume the next argument
						iterator.next();
					}
				} else {
					throw new ProgramErrorException("Keyword argument not found in '" + functionClassName + "' function definition: :" + keywordArgument.getName());
				}
			} else if (!keysToBindings.isEmpty()) {
				throw new ProgramErrorException("Expected Keyword argument for call to '" + functionClassName + " was: " + nextArgument);
			} else if (restBinding == null) {
				final int numberOfOptionals = optionalBindings.size();
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				throw new ProgramErrorException("Too many arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at most " + maxNumberProvided + " accepted.");
			}
		}

		for (final KeyBinding keyBinding : keysToBindings.values()) {
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getSymbolStruct();
			symbolsToBind.put(suppliedPSymbol, NILStruct.INSTANCE);
		}

		if (restBinding != null) {
			final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
			final LispStruct restListStruct = ListStruct.buildProperList(restList);
			symbolsToBind.put(restSymbol, restListStruct);
		}

		return symbolsToBind;
	}

	private static KeywordStruct getKeywordStruct(final String symbolName) {

		final PackageSymbolStruct symbol = GlobalPackageStruct.KEYWORD.findSymbol(symbolName);
		if (symbol == null) {
			return new KeywordStruct(symbolName);
		}
		// NOTE: This should be a safe cast because we're finding the symbol in the Keyword Package and they are only
		//       this type of symbol.
		return (KeywordStruct) symbol.getSymbol();
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
