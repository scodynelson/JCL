package jcl.compiler.real.sa.analyzer.specialoperator.lambda;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

final class LambdaListParser {

	private static final SymbolStruct<?> AND_OPTIONAL = GlobalPackageStruct.KEYWORD.intern("&OPTIONAL").getSymbolStruct();
	private static final SymbolStruct<?> AND_REST = GlobalPackageStruct.KEYWORD.intern("&REST").getSymbolStruct();
	private static final SymbolStruct<?> AND_KEY = GlobalPackageStruct.KEYWORD.intern("&KEY").getSymbolStruct();
	private static final SymbolStruct<?> AND_ALLOW_OTHER_KEYS = GlobalPackageStruct.KEYWORD.intern("&ALLOW-OTHER-KEYS").getSymbolStruct();
	private static final SymbolStruct<?> AND_AUX = GlobalPackageStruct.KEYWORD.intern("&AUX").getSymbolStruct();

	private static final SymbolStruct<?> AND_WHOLE = GlobalPackageStruct.KEYWORD.intern("&WHOLE").getSymbolStruct();
	private static final SymbolStruct<?> AND_BODY = GlobalPackageStruct.KEYWORD.intern("&BODY").getSymbolStruct();
	private static final SymbolStruct<?> AND_ENVIRONMENT = GlobalPackageStruct.KEYWORD.intern("&ENVIRONMENT").getSymbolStruct();

	private LambdaListParser() {
	}

	public static OrdinaryLambdaListBindings parseOrdinaryLambdaList(final SemanticAnalyzer semanticAnalyzer,
	                                                                 final AnalysisBuilder analysisBuilder,
	                                                                 final ListStruct lambdaList,
	                                                                 final DeclareElement declareElement) {

		final List<LispStruct> lambdaListJava = lambdaList.getAsJavaList();
		final Iterator<LispStruct> iterator = lambdaListJava.iterator();

		LispStruct currentElement = null;
		int position = 0;

		List<RequiredBinding> requiredBindings = Collections.emptyList();
		if (iterator.hasNext()) {
			final RequiredParseResult requiredParseResult
					= parseRequiredBindings(analysisBuilder, iterator, position, declareElement);

			requiredBindings = requiredParseResult.getRequiredBindings();
			currentElement = requiredParseResult.getCurrentElement();
			position = requiredParseResult.getCurrentPosition();
		}

		List<OptionalBinding> optionalBindings = Collections.emptyList();
		if (AND_OPTIONAL.equals(currentElement)) {
			final OptionalParseResult optionalParseResult
					= parseOptionalBindings(semanticAnalyzer, analysisBuilder, iterator, position, declareElement);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		RestBinding restBinding = null;
		if (AND_REST.equals(currentElement)) {
			final RestParseResult restParseResult
					= parseRestBinding(analysisBuilder, iterator, position, declareElement);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
			position = restParseResult.getCurrentPosition();
		}

		List<KeyBinding> keyBindings = Collections.emptyList();
		boolean allowOtherKeys = false;
		if (AND_KEY.equals(currentElement)) {
			final KeyParseResult keyParseResult
					= parseKeyBindings(semanticAnalyzer, analysisBuilder, iterator, position, declareElement);

			keyBindings = keyParseResult.getKeyBindings();
			allowOtherKeys = keyParseResult.isAllowOtherKeys();
			currentElement = keyParseResult.getCurrentElement();
			position = keyParseResult.getCurrentPosition();
		}

		List<AuxBinding> auxBindings = Collections.emptyList();
		if (AND_AUX.equals(currentElement)) {
			final AuxParseResult auxParseResult
					= parseAuxBindings(semanticAnalyzer, analysisBuilder, iterator, position, declareElement);

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

	private static RequiredParseResult parseRequiredBindings(final AnalysisBuilder analysisBuilder, final Iterator<LispStruct> iterator,
	                                                         final int position, final DeclareElement declareElement) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final List<RequiredBinding> requiredBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (!(currentElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("LambdaList required parameters must be of type SymbolStruct: " + currentElement);
			}
			final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
			final ParameterAllocation requiredAllocation = new ParameterAllocation(currentPosition++);
			final RequiredBinding requiredBinding = new RequiredBinding(currentParam, requiredAllocation);
			requiredBindings.add(requiredBinding);

			currentElement = iterator.next();

			final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
			analysisBuilder.setBindingsPosition(newBindingsPosition);

			final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
			final boolean isSpecial = isSpecial(declareElement, currentParam);

			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullElement.INSTANCE);
			if (isSpecial) {
				currentEnvironment.addDynamicBinding(binding);
			} else {
				currentEnvironment.addLexicalBinding(binding);
			}
		}

		return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
	}

	private static OptionalParseResult parseOptionalBindings(final SemanticAnalyzer semanticAnalyzer, final AnalysisBuilder analysisBuilder,
	                                                         final Iterator<LispStruct> iterator, final int position,
	                                                         final DeclareElement declareElement) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final List<OptionalBinding> optionalBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final ParameterAllocation optionalAllocation = new ParameterAllocation(currentPosition++);
				final OptionalBinding optionalBinding = new OptionalBinding(currentParam, optionalAllocation, null, null);
				optionalBindings.add(optionalBinding);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullElement.INSTANCE);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
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
				final SymbolStruct<?> varNameCurrent = (SymbolStruct<?>) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment currentEnvironment1 = environmentStack.pop();
				final Element parameterValueInitForm = semanticAnalyzer.analyzeForm(initForm, analysisBuilder);
				environmentStack.push(currentEnvironment1);

				int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				boolean isSpecial = isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new ProgramErrorException("LambdaList optional supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct<?>) thirdInCurrent;
					final ParameterAllocation suppliedPAllocation = new ParameterAllocation(currentPosition++);
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, suppliedPAllocation);

					newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
					analysisBuilder.setBindingsPosition(newBindingsPosition);

					allocation = new ParameterAllocation(newBindingsPosition);
					isSpecial = isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, allocation, T.INSTANCE, NullElement.INSTANCE);
					if (isSpecial) {
						currentEnvironment.addDynamicBinding(binding);
					} else {
						currentEnvironment.addLexicalBinding(binding);
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

	private static RestParseResult parseRestBinding(final AnalysisBuilder analysisBuilder, final Iterator<LispStruct> iterator,
	                                                final int position, final DeclareElement declareElement) {

		int currentPosition = position;

		final LispStruct currentElement = iterator.next();
		if (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			throw new ProgramErrorException("LambdaList rest parameter must only have 1 parameter: " + iterator.next());
		}

		if (!(currentElement instanceof SymbolStruct)) {
			throw new ProgramErrorException("LambdaList rest parameters must be of type SymbolStruct: " + currentElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final boolean isSpecial = isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullElement.INSTANCE);
		if (isSpecial) {
			currentEnvironment.addDynamicBinding(binding);
		} else {
			currentEnvironment.addLexicalBinding(binding);
		}

		final ParameterAllocation restAllocation = new ParameterAllocation(currentPosition++);
		final RestBinding restBinding = new RestBinding(currentParam, restAllocation);
		return new RestParseResult(currentElement, currentPosition, restBinding);
	}

	private static KeyParseResult parseKeyBindings(final SemanticAnalyzer semanticAnalyzer, final AnalysisBuilder analysisBuilder,
	                                               final Iterator<LispStruct> iterator, final int position,
	                                               final DeclareElement declareElement) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final List<KeyBinding> keyBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final KeywordSymbolStruct keyName = GlobalPackageStruct.KEYWORD.intern(currentParam.getName()).getPackageSymbolType();
				final ParameterAllocation keyAllocation = new ParameterAllocation(currentPosition++);
				final KeyBinding keyBinding = new KeyBinding(currentParam, keyAllocation, null, keyName, null);
				keyBindings.add(keyBinding);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullElement.INSTANCE);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
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
					varNameCurrent = (SymbolStruct<?>) firstInCurrent;
					varKeyNameCurrent = GlobalPackageStruct.KEYWORD.intern(varNameCurrent.getName()).getPackageSymbolType();
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
					varNameCurrent = (SymbolStruct<?>) secondInCurrentVar;
				} else {
					throw new ProgramErrorException("LambdaList key var name parameters must be of type SymbolStruct or ListStruct: " + firstInCurrent);
				}

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment currentEnvironment1 = environmentStack.pop();
				final Element parameterValueInitForm = semanticAnalyzer.analyzeForm(initForm, analysisBuilder);
				environmentStack.push(currentEnvironment1);

				int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				boolean isSpecial = isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new ProgramErrorException("LambdaList key supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct<?>) thirdInCurrent;
					final ParameterAllocation suppliedPAllocation = new ParameterAllocation(currentPosition++);
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, suppliedPAllocation);

					newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
					analysisBuilder.setBindingsPosition(newBindingsPosition);

					allocation = new ParameterAllocation(newBindingsPosition);
					isSpecial = isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, allocation, T.INSTANCE, NullElement.INSTANCE);
					if (isSpecial) {
						currentEnvironment.addDynamicBinding(binding);
					} else {
						currentEnvironment.addLexicalBinding(binding);
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

	private static AuxParseResult parseAuxBindings(final SemanticAnalyzer semanticAnalyzer, final AnalysisBuilder analysisBuilder,
	                                               final Iterator<LispStruct> iterator, final int position,
	                                               final DeclareElement declareElement) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final List<AuxBinding> auxBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final ParameterAllocation auxAllocation = new ParameterAllocation(currentPosition++);
				final AuxBinding auxBinding = new AuxBinding(currentParam, auxAllocation, null);
				auxBindings.add(auxBinding);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, allocation, T.INSTANCE, NullElement.INSTANCE);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
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
				final SymbolStruct<?> varNameCurrent = (SymbolStruct<?>) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final ParameterAllocation auxAllocation = new ParameterAllocation(currentPosition++);
				final AuxBinding auxBinding = new AuxBinding(varNameCurrent, auxAllocation, initForm);
				auxBindings.add(auxBinding);

				// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
				final Environment currentEnvironment1 = environmentStack.pop();
				final Element parameterValueInitForm = semanticAnalyzer.analyzeForm(initForm, analysisBuilder);
				environmentStack.push(currentEnvironment1);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analysisBuilder.setBindingsPosition(newBindingsPosition);

				final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
				final boolean isSpecial = isSpecial(declareElement, varNameCurrent);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, allocation, T.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					currentEnvironment.addDynamicBinding(binding);
				} else {
					currentEnvironment.addLexicalBinding(binding);
				}
			} else {
				throw new ProgramErrorException("LambdaList aux parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		return new AuxParseResult(currentElement, currentPosition, auxBindings);
	}

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolStruct<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolElement<?> specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar.getSymbolStruct())) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
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
