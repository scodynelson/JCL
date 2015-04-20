package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.CompilerConstants;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.BodyBinding;
import jcl.compiler.real.environment.binding.lambdalist.EnvironmentBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.environment.binding.lambdalist.WholeBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.printer.Printer;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;

public class LambdaListParser {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	protected WholeParseResult parseWholeBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                             final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		final LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			final String printedElement = printer.print(currentElement);
			throw new ProgramErrorException("LambdaList &whole parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final WholeBinding wholeBinding = new WholeBinding(currentParam);
		return new WholeParseResult(currentPosition, wholeBinding);
	}

	protected EnvironmentParseResult parseEnvironmentBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                                         final int position, final DeclareStruct declareElement,
	                                                         final boolean isAfterRequired) {

		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			final String printedElement = printer.print(currentElement);
			throw new ProgramErrorException("LambdaList &environment parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		if (iterator.hasNext() && isAfterRequired) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &environment parameter must only have 1 parameter: " + printedElement);
			}
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
		environment.addDynamicBinding(binding);

		final EnvironmentBinding environmentBinding = new EnvironmentBinding(currentParam);
		return new EnvironmentParseResult(currentElement, currentPosition, environmentBinding);
	}

	protected RequiredParseResult parseRequiredBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final int position, final DeclareStruct declareElement,
	                                                    final boolean isDotted) {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (!iterator.hasNext() && isDotted) {
				return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
			}
			if (isLambdaListKeyword(currentElement)) {
				return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
			}

			if (!(currentElement instanceof SymbolStruct)) {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList required parameters must be a symbol: " + printedElement);
			}
			final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
			final RequiredBinding requiredBinding = new RequiredBinding(currentParam);
			requiredBindings.add(requiredBinding);

			final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
			final int newBindingsPosition = currentLambda.getNextParameterNumber();
			environment.setBindingsPosition(newBindingsPosition);

			final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
			if (isSpecial) {
				environment.addDynamicBinding(binding);
			} else {
				environment.addLexicalBinding(binding);
			}
		} while (iterator.hasNext());

		return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
	}

	protected OptionalParseResult parseOptionalBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final int position, final DeclareStruct declareElement,
	                                                    final boolean isDotted) {

		final List<OptionalBinding> optionalBindings = new ArrayList<>();
		int currentPosition = position;

		if (!iterator.hasNext()) {
			return new OptionalParseResult(null, currentPosition, optionalBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new OptionalParseResult(currentElement, currentPosition, optionalBindings);
			}
			if (!iterator.hasNext() && isDotted) {
				return new OptionalParseResult(currentElement, currentPosition, optionalBindings);
			}

			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final SymbolStruct<?> customSuppliedPCurrent = new SymbolStruct<>(customSuppliedPName, GlobalPackageStruct.SYSTEM);

				final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

				newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				isSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

				binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final OptionalBinding optionalBinding = new OptionalBinding(currentParam, NullStruct.INSTANCE, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					final String printedElement = printer.print(currentParam);
					throw new ProgramErrorException("LambdaList &optional parameters must have between 1 and 3 parameters: " + printedElement);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					final String printedElement = printer.print(firstInCurrent);
					throw new ProgramErrorException("LambdaList &optional var name parameters must be a symbol: " + printedElement);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct) firstInCurrent;

				LispStruct initForm = NullStruct.INSTANCE;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, environment);

				LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, TType.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPBinding suppliedPBinding;
				if (thirdInCurrent.equals(NullStruct.INSTANCE)) {
					final String paramName = varNameCurrent.getName();
					final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
					final SymbolStruct<?> customSuppliedPCurrent = new SymbolStruct<>(customSuppliedPName, GlobalPackageStruct.SYSTEM);

					suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					isSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

					binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				} else {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						final String printedElement = printer.print(thirdInCurrent);
						throw new ProgramErrorException("LambdaList &optional supplied-p parameters must be a symbol: " + printedElement);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent);

					currentLambda = Environments.getEnclosingLambda(environment);
					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					isSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				}

				final OptionalBinding optionalBinding = new OptionalBinding(varNameCurrent, initForm, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &optional parameters must be a symbol or a list: " + printedElement);
			}
		} while (iterator.hasNext());

		return new OptionalParseResult(currentElement, currentPosition, optionalBindings);
	}

	protected RestParseResult parseRestBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                           final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &rest parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			final String printedElement = printer.print(currentElement);
			throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		if (iterator.hasNext()) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &rest parameter must only have 1 parameter: " + printedElement);
			}
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestBinding restBinding = new RestBinding(currentParam);
		return new RestParseResult(currentElement, currentPosition, restBinding);
	}

	protected RestParseResult parseDottedRestBinding(final Environment environment, final LispStruct dottedRest,
	                                                 final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		if (!(dottedRest instanceof SymbolStruct)) {
			final String printedElement = printer.print(dottedRest);
			throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) dottedRest;

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestBinding restBinding = new RestBinding(currentParam);
		return new RestParseResult(dottedRest, currentPosition, restBinding);
	}

	protected BodyParseResult parseBodyBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                           final int position, final DeclareStruct declareElement) {

		int currentPosition = position;

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &body parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			final String printedElement = printer.print(currentElement);
			throw new ProgramErrorException("LambdaList &body parameters must be a symbol: " + printedElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

		if (iterator.hasNext()) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &body parameter must only have 1 parameter: " + printedElement);
			}
		}

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		environment.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final BodyBinding bodyBinding = new BodyBinding(currentParam);
		return new BodyParseResult(currentElement, currentPosition, bodyBinding);
	}

	protected KeyParseResult parseKeyBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                          final int position, final DeclareStruct declareElement) {

		final List<KeyBinding> keyBindings = new ArrayList<>();
		int currentPosition = position;

		if (!iterator.hasNext()) {
			return new KeyParseResult(null, currentPosition, keyBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new KeyParseResult(currentElement, currentPosition, keyBindings);
			}

			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final KeywordStruct keyName = getKeywordStruct(currentParam.getName());

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final SymbolStruct<?> customSuppliedPCurrent = new SymbolStruct<>(customSuppliedPName, GlobalPackageStruct.SYSTEM);

				final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

				newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				isSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

				binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final KeyBinding keyBinding = new KeyBinding(currentParam, NullStruct.INSTANCE, keyName, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					final String printedElement = printer.print(currentParam);
					throw new ProgramErrorException("LambdaList &key parameters must have between 1 and 3 parameters: " + printedElement);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				final SymbolStruct<?> varNameCurrent;
				final KeywordStruct varKeyNameCurrent;
				if (firstInCurrent instanceof SymbolStruct) {
					varNameCurrent = (SymbolStruct) firstInCurrent;
					varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
				} else if (firstInCurrent instanceof ListStruct) {
					final ListStruct currentVar = (ListStruct) firstInCurrent;
					if (currentVar.size() != 2) {
						final String printedElement = printer.print(currentVar);
						throw new ProgramErrorException("LambdaList &key var name list parameters must have 2 parameters: " + printedElement);
					}

					final LispStruct firstInCurrentVar = currentVar.getFirst();
					if (firstInCurrentVar instanceof KeywordStruct) {
						varKeyNameCurrent = (KeywordStruct) firstInCurrentVar;
					} else if (firstInCurrentVar instanceof SymbolStruct) {
						final SymbolStruct<?> keyNameSymbol = (SymbolStruct) firstInCurrentVar;
						varKeyNameCurrent = getKeywordStruct(keyNameSymbol.getName());
					} else {
						final String printedElement = printer.print(firstInCurrentVar);
						throw new ProgramErrorException("LambdaList &key var name list key-name parameters must be a keyword or a symbol: " + printedElement);
					}

					final LispStruct secondInCurrentVar = currentVar.getRest().getFirst();
					if (!(secondInCurrentVar instanceof SymbolStruct)) {
						final String printedElement = printer.print(secondInCurrentVar);
						throw new ProgramErrorException("LambdaList &key var name list name parameters must be a symbol: " + printedElement);
					}
					varNameCurrent = (SymbolStruct) secondInCurrentVar;
				} else {
					final String printedElement = printer.print(firstInCurrent);
					throw new ProgramErrorException("LambdaList &key var name parameters must be a symbol or a list: " + printedElement);
				}

				LispStruct initForm = NullStruct.INSTANCE;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, environment);

				LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, TType.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPBinding suppliedPBinding;
				if (thirdInCurrent.equals(NullStruct.INSTANCE)) {
					final String paramName = varNameCurrent.getName();
					final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
					final SymbolStruct<?> customSuppliedPCurrent = new SymbolStruct<>(customSuppliedPName, GlobalPackageStruct.SYSTEM);

					suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					isSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

					binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				} else {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						final String printedElement = printer.print(thirdInCurrent);
						throw new ProgramErrorException("LambdaList &key supplied-p parameters must be a symbol: " + printedElement);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent);

					currentLambda = Environments.getEnclosingLambda(environment);
					newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					isSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

					binding = new EnvironmentParameterBinding(suppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}
				}

				final KeyBinding keyBinding = new KeyBinding(varNameCurrent, initForm, varKeyNameCurrent, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &key parameters must be a symbol or a list: " + printedElement);
			}

		} while (iterator.hasNext());

		return new KeyParseResult(currentElement, currentPosition, keyBindings);
	}

	protected AuxParseResult parseAuxBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                          final int position, final DeclareStruct declareElement) {

		final List<AuxBinding> auxBindings = new ArrayList<>();
		int currentPosition = position;

		if (!iterator.hasNext()) {
			return new AuxParseResult(null, currentPosition, auxBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new AuxParseResult(currentElement, currentPosition, auxBindings);
			}

			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final AuxBinding auxBinding = new AuxBinding(currentParam, NullStruct.INSTANCE);
				auxBindings.add(auxBinding);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 2)) {
					final String printedElement = printer.print(currentParam);
					throw new ProgramErrorException("LambdaList &aux parameters must have between 1 and 3 parameters: " + printedElement);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					final String printedElement = printer.print(firstInCurrent);
					throw new ProgramErrorException("LambdaList &aux var name parameters must be a symbol: " + printedElement);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct) firstInCurrent;

				LispStruct initForm = NullStruct.INSTANCE;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final AuxBinding auxBinding = new AuxBinding(varNameCurrent, initForm);
				auxBindings.add(auxBinding);

				final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, environment);

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				final int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

				final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(varNameCurrent, TType.INSTANCE, parameterValueInitForm);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &aux parameters must be a symbol or a list: " + printedElement);
			}
		} while (iterator.hasNext());

		return new AuxParseResult(currentElement, currentPosition, auxBindings);
	}

	protected static boolean isLambdaListKeyword(final LispStruct lispStruct) {
		return lispStruct.equals(CompilerConstants.AUX)
				|| lispStruct.equals(CompilerConstants.ALLOW_OTHER_KEYS)
				|| lispStruct.equals(CompilerConstants.KEY)
				|| lispStruct.equals(CompilerConstants.OPTIONAL)
				|| lispStruct.equals(CompilerConstants.REST)
				|| lispStruct.equals(CompilerConstants.WHOLE)
				|| lispStruct.equals(CompilerConstants.ENVIRONMENT)
				|| lispStruct.equals(CompilerConstants.BODY);
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
}
