package jcl.compiler.real.sa.analyzer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class LambdaListParser {

	// TODO: fix these!!!
	private static final SymbolStruct<?> AND_OPTIONAL = GlobalPackageStruct.KEYWORD.intern("&OPTIONAL").getSymbol();
	private static final SymbolStruct<?> AND_REST = GlobalPackageStruct.KEYWORD.intern("&REST").getSymbol();
	private static final SymbolStruct<?> AND_KEY = GlobalPackageStruct.KEYWORD.intern("&KEY").getSymbol();
	private static final SymbolStruct<?> AND_ALLOW_OTHER_KEYS = GlobalPackageStruct.KEYWORD.intern("&ALLOW-OTHER-KEYS").getSymbol();
	private static final SymbolStruct<?> AND_AUX = GlobalPackageStruct.KEYWORD.intern("&AUX").getSymbol();

	private static final SymbolStruct<?> AND_WHOLE = GlobalPackageStruct.KEYWORD.intern("&WHOLE").getSymbol();
	private static final SymbolStruct<?> AND_BODY = GlobalPackageStruct.KEYWORD.intern("&BODY").getSymbol();
	private static final SymbolStruct<?> AND_ENVIRONMENT = GlobalPackageStruct.KEYWORD.intern("&ENVIRONMENT").getSymbol();

	private LambdaListParser() {
	}

	public static OrdinaryLambdaListBindings parseOrdinaryLambdaList(final FormAnalyzer formAnalyzer,
	                                                                 final Environment environment,
	                                                                 final ListStruct lambdaList,
	                                                                 final DeclareStruct declareElement) {

		final List<? extends LispStruct> lambdaListJava = lambdaList.getAsJavaList();

		final Iterator<? extends LispStruct> iterator = lambdaListJava.iterator();

		LispStruct currentElement = null;
		int position = 0;

		List<RequiredBinding> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= parseRequiredBindings(environment, iterator, position, declareElement);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
			position = requiredParseResult.getCurrentPosition();
		}

		List<OptionalBinding> optionalBindings = Collections.emptyList();
		if (AND_OPTIONAL.equals(currentElement)) {
			final OptionalParseResult optionalParseResult
					= parseOptionalBindings(formAnalyzer, environment, iterator, position, declareElement);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		RestBinding restBinding = null;
		if (AND_REST.equals(currentElement)) {
			final RestParseResult restParseResult
					= parseRestBinding(environment, iterator, position, declareElement);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
			position = restParseResult.getCurrentPosition();
		}

		List<KeyBinding> keyBindings = Collections.emptyList();
		boolean allowOtherKeys = false;
		if (AND_KEY.equals(currentElement)) {
			final KeyParseResult keyParseResult
					= parseKeyBindings(formAnalyzer, environment, iterator, position, declareElement);

			keyBindings = keyParseResult.getKeyBindings();
			allowOtherKeys = keyParseResult.isAllowOtherKeys();
			currentElement = keyParseResult.getCurrentElement();
			position = keyParseResult.getCurrentPosition();
		}

		List<AuxBinding> auxBindings = Collections.emptyList();
		if (AND_AUX.equals(currentElement)) {
			final AuxParseResult auxParseResult
					= parseAuxBindings(formAnalyzer, environment, iterator, position, declareElement);

			auxBindings = auxParseResult.getAuxBindings();
		}

		if (iterator.hasNext()) {
			throw new ProgramErrorException("Unexpected element at the end of Ordinary Lambda List: " + iterator.next());
		}

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	/*
	 * BINDING PARSE METHODS
	 */

	private static RequiredParseResult parseRequiredBindings(final Environment environment, final Iterator<? extends LispStruct> iterator,
	                                                         final int position, final DeclareStruct declareElement) {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (!(currentElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("LambdaList required parameters must be of type SymbolStruct: " + currentElement);
			}
			final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
			final ParameterAllocation requiredAllocation = new ParameterAllocation(currentPosition++);
			final RequiredBinding requiredBinding = new RequiredBinding(currentParam, requiredAllocation);
			requiredBindings.add(requiredBinding);

			currentElement = iterator.next();

			final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
			final int newBindingsPosition = currentLambda.getNextParameterNumber();
			environment.setBindingsPosition(newBindingsPosition);

			final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
			final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
			if (isSpecial) {
				environment.addDynamicBinding(binding);
			} else {
				environment.addLexicalBinding(binding);
			}
		}

		return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
	}

	private static OptionalParseResult parseOptionalBindings(final FormAnalyzer formAnalyzer, final Environment environment,
	                                                         final Iterator<? extends LispStruct> iterator, final int position,
	                                                         final DeclareStruct declareElement) {

		final List<OptionalBinding> optionalBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final ParameterAllocation optionalAllocation = new ParameterAllocation(currentPosition++);
				final OptionalBinding optionalBinding = new OptionalBinding(currentParam, optionalAllocation, null, null);
				optionalBindings.add(optionalBinding);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					throw new ProgramErrorException("LambdaList optional parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					throw new ProgramErrorException("LambdaList optional var name parameters must be of type SymbolStruct: " + firstInCurrent);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment parentEnvironment = environment.getParent();
				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, parentEnvironment);

				LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new ProgramErrorException("LambdaList optional supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;
					final ParameterAllocation suppliedPAllocation = new ParameterAllocation(currentPosition++);
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, suppliedPAllocation);

					currentLambda = Environments.getEnclosingLambda(environment);
					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					allocation = new ParameterAllocation(newBindingsPosition);
					isSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, allocation, T.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				}

				final ParameterAllocation optionalAllocation = new ParameterAllocation(currentPosition++);
				final OptionalBinding optionalBinding = new OptionalBinding(varNameCurrent, optionalAllocation, initForm, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else {
				throw new ProgramErrorException("LambdaList optional parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		return new OptionalParseResult(currentElement, currentPosition, optionalBindings);
	}

	private static RestParseResult parseRestBinding(final Environment environment, final Iterator<? extends LispStruct> iterator,
	                                                final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		final LispStruct currentElement = iterator.next();
		if (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			throw new ProgramErrorException("LambdaList rest parameter must only have 1 parameter: " + iterator.next());
		}

		if (!(currentElement instanceof SymbolStruct)) {
			throw new ProgramErrorException("LambdaList rest parameters must be of type SymbolStruct: " + currentElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final ParameterAllocation restAllocation = new ParameterAllocation(currentPosition++);
		final RestBinding restBinding = new RestBinding(currentParam, restAllocation);
		return new RestParseResult(currentElement, currentPosition, restBinding);
	}

	private static KeyParseResult parseKeyBindings(final FormAnalyzer formAnalyzer, final Environment environment,
	                                               final Iterator<? extends LispStruct> iterator, final int position,
	                                               final DeclareStruct declareElement) {

		final List<KeyBinding> keyBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final KeywordSymbolStruct keyName = new KeywordSymbolStruct(currentParam.getName());
				final ParameterAllocation keyAllocation = new ParameterAllocation(currentPosition++);
				final KeyBinding keyBinding = new KeyBinding(currentParam, keyAllocation, null, keyName, null);
				keyBindings.add(keyBinding);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					throw new ProgramErrorException("LambdaList key parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				final SymbolStruct<?> varNameCurrent;
				final KeywordSymbolStruct varKeyNameCurrent;
				if (firstInCurrent instanceof SymbolStruct) {
					varNameCurrent = (SymbolStruct) firstInCurrent;
					varKeyNameCurrent = new KeywordSymbolStruct(varNameCurrent.getName());
				} else if (firstInCurrent instanceof ListStruct) {
					final ListStruct currentVar = (ListStruct) firstInCurrent;
					if (currentVar.size() != 2) {
						throw new ProgramErrorException("LambdaList key var name list parameters must have 2 parameters: " + currentVar);
					}

					final LispStruct firstInCurrentVar = currentVar.getFirst();
					if (!(firstInCurrentVar instanceof KeywordSymbolStruct)) {
						throw new ProgramErrorException("LambdaList key var name list key-name parameters must be of type KeywordStruct: " + firstInCurrentVar);
					}
					varKeyNameCurrent = (KeywordSymbolStruct) firstInCurrentVar;

					final LispStruct secondInCurrentVar = currentVar.getRest().getFirst();
					if (!(secondInCurrentVar instanceof SymbolStruct)) {
						throw new ProgramErrorException("LambdaList key var name list name parameters must be of type SymbolStruct: " + secondInCurrentVar);
					}
					varNameCurrent = (SymbolStruct) secondInCurrentVar;
				} else {
					throw new ProgramErrorException("LambdaList key var name parameters must be of type SymbolStruct or ListStruct: " + firstInCurrent);
				}

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment parentEnvironment = environment.getParent();
				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, parentEnvironment);

				LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new ProgramErrorException("LambdaList key supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;
					final ParameterAllocation suppliedPAllocation = new ParameterAllocation(currentPosition++);
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, suppliedPAllocation);

					currentLambda = Environments.getEnclosingLambda(environment);
					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					allocation = new ParameterAllocation(newBindingsPosition);
					isSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, allocation, T.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				}

				final ParameterAllocation keyAllocation = new ParameterAllocation(currentPosition++);
				final KeyBinding keyBinding = new KeyBinding(varNameCurrent, keyAllocation, initForm, varKeyNameCurrent, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else {
				throw new ProgramErrorException("LambdaList key parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		boolean allowOtherKeys = false;
		if (currentElement.equals(AND_ALLOW_OTHER_KEYS)) {
			allowOtherKeys = true;
			currentElement = iterator.next();
		}

		return new KeyParseResult(currentElement, currentPosition, keyBindings, allowOtherKeys);
	}

	private static AuxParseResult parseAuxBindings(final FormAnalyzer formAnalyzer, final Environment environment,
	                                               final Iterator<? extends LispStruct> iterator, final int position,
	                                               final DeclareStruct declareElement) {

		final List<AuxBinding> auxBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final ParameterAllocation auxAllocation = new ParameterAllocation(currentPosition++);
				final AuxBinding auxBinding = new AuxBinding(currentParam, auxAllocation, null);
				auxBindings.add(auxBinding);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 2)) {
					throw new ProgramErrorException("LambdaList aux parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					throw new ProgramErrorException("LambdaList aux var name parameters must be of type SymbolStruct: " + firstInCurrent);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final ParameterAllocation auxAllocation = new ParameterAllocation(currentPosition++);
				final AuxBinding auxBinding = new AuxBinding(varNameCurrent, auxAllocation, initForm);
				auxBindings.add(auxBinding);

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment parentEnvironment = environment.getParent();
				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, parentEnvironment);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else {
				throw new ProgramErrorException("LambdaList aux parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		return new AuxParseResult(currentElement, currentPosition, auxBindings);
	}

	/*
	 * BINDING PARSE RESULT OBJECTS
	 */

	private static class ParseResult {

		private final LispStruct currentElement;
		private final int currentPosition;

		ParseResult(final LispStruct currentElement, final int currentPosition) {
			this.currentElement = currentElement;
			this.currentPosition = currentPosition;
		}

		public LispStruct getCurrentElement() {
			return currentElement;
		}

		public int getCurrentPosition() {
			return currentPosition;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(currentElement)
			                            .append(currentPosition)
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
			final ParseResult rhs = (ParseResult) obj;
			return new EqualsBuilder().append(currentElement, rhs.currentElement)
			                          .append(currentPosition, rhs.currentPosition)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(currentElement)
			                                                                .append(currentPosition)
			                                                                .toString();
		}
	}

	private static final class RequiredParseResult extends ParseResult {

		private final List<RequiredBinding> requiredBindings;

		private RequiredParseResult(final LispStruct currentElement, final int currentPosition, final List<RequiredBinding> requiredBindings) {
			super(currentElement, currentPosition);
			this.requiredBindings = requiredBindings;
		}

		public List<RequiredBinding> getRequiredBindings() {
			return requiredBindings;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(requiredBindings)
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
			final RequiredParseResult rhs = (RequiredParseResult) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(requiredBindings, rhs.requiredBindings)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(requiredBindings)
			                                                                .toString();
		}
	}

	private static final class OptionalParseResult extends ParseResult {

		private final List<OptionalBinding> optionalBindings;

		private OptionalParseResult(final LispStruct currentElement, final int currentPosition, final List<OptionalBinding> optionalBindings) {
			super(currentElement, currentPosition);
			this.optionalBindings = optionalBindings;
		}

		public List<OptionalBinding> getOptionalBindings() {
			return optionalBindings;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(optionalBindings)
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
			final OptionalParseResult rhs = (OptionalParseResult) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(optionalBindings, rhs.optionalBindings)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(optionalBindings)
			                                                                .toString();
		}
	}

	private static final class RestParseResult extends ParseResult {

		private final RestBinding restBinding;

		private RestParseResult(final LispStruct currentElement, final int currentPosition, final RestBinding restBinding) {
			super(currentElement, currentPosition);
			this.restBinding = restBinding;
		}

		public RestBinding getRestBinding() {
			return restBinding;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(restBinding)
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
			final RestParseResult rhs = (RestParseResult) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(restBinding, rhs.restBinding)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(restBinding)
			                                                                .toString();
		}
	}

	private static final class KeyParseResult extends ParseResult {

		private final List<KeyBinding> keyBindings;
		private final boolean allowOtherKeys;

		private KeyParseResult(final LispStruct currentElement, final int currentPosition, final List<KeyBinding> keyBindings,
		                       final boolean allowOtherKeys) {
			super(currentElement, currentPosition);
			this.keyBindings = keyBindings;
			this.allowOtherKeys = allowOtherKeys;
		}

		public List<KeyBinding> getKeyBindings() {
			return keyBindings;
		}

		public boolean isAllowOtherKeys() {
			return allowOtherKeys;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(keyBindings)
			                            .append(allowOtherKeys)
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
			final KeyParseResult rhs = (KeyParseResult) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(keyBindings, rhs.keyBindings)
			                          .append(allowOtherKeys, rhs.allowOtherKeys)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(keyBindings)
			                                                                .append(allowOtherKeys)
			                                                                .toString();
		}
	}

	private static final class AuxParseResult extends ParseResult {

		private final List<AuxBinding> auxBindings;

		private AuxParseResult(final LispStruct currentElement, final int currentPosition, final List<AuxBinding> auxBindings) {
			super(currentElement, currentPosition);
			this.auxBindings = auxBindings;
		}

		public List<AuxBinding> getAuxBindings() {
			return auxBindings;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(auxBindings)
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
			final AuxParseResult rhs = (AuxParseResult) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(auxBindings, rhs.auxBindings)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(auxBindings)
			                                                                .toString();
		}
	}

	/*
	 * UTILITY METHODS
	 */

	private static boolean isLambdaListKeyword(final LispStruct lispStruct) {
		return lispStruct.equals(AND_AUX)
				|| lispStruct.equals(AND_ALLOW_OTHER_KEYS)
				|| lispStruct.equals(AND_KEY)
				|| lispStruct.equals(AND_OPTIONAL)
				|| lispStruct.equals(AND_REST)
				|| lispStruct.equals(AND_WHOLE)
				|| lispStruct.equals(AND_ENVIRONMENT)
				|| lispStruct.equals(AND_BODY);
	}
}
