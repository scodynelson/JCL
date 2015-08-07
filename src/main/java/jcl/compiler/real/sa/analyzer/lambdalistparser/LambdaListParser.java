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
import jcl.compiler.real.environment.binding.lambdalist.DestructuringLambdaListBindings;
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
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.printer.Printer;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.springframework.beans.factory.annotation.Autowired;

public class LambdaListParser {

	@Autowired
	private DestructuringLambdaListParser destructuringLambdaListParser;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	protected WholeParseResult parseWholeBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                             final DeclareStruct declareElement) {

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

		final WholeBinding wholeBinding = new WholeBinding(currentParam, isSpecial);
		return new WholeParseResult(wholeBinding);
	}

	protected EnvironmentParseResult parseEnvironmentBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                                         final boolean isAfterRequired) {

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
		return new EnvironmentParseResult(currentElement, environmentBinding);
	}

	protected RequiredParseResult parseRequiredBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final DeclareStruct declareElement, final boolean isDotted,
	                                                    final boolean isDestructuringAllowed) {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (!iterator.hasNext() && isDotted) {
				return new RequiredParseResult(currentElement, requiredBindings);
			}
			if (isLambdaListKeyword(currentElement)) {
				return new RequiredParseResult(currentElement, requiredBindings);
			}

			final SymbolStruct<?> currentParam;
			DestructuringLambdaListBindings destructuringForm = null;
			if (currentElement instanceof SymbolStruct) {
				currentParam = (SymbolStruct) currentElement;
			} else {
				if (isDestructuringAllowed) {
					if (currentElement instanceof ListStruct) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
					} else {
						final String printedElement = printer.print(currentElement);
						throw new ProgramErrorException("LambdaList required parameter must be a symbol or a destructuring list: " + printedElement);
					}
				} else {
					final String printedElement = printer.print(currentElement);
					throw new ProgramErrorException("LambdaList required parameters must be a symbol: " + printedElement);
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

			final RequiredBinding requiredBinding = new RequiredBinding(currentParam, destructuringForm, isSpecial);
			requiredBindings.add(requiredBinding);
		} while (iterator.hasNext());

		return new RequiredParseResult(currentElement, requiredBindings);
	}

	protected OptionalParseResult parseOptionalBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final DeclareStruct declareElement, final boolean isDotted,
	                                                    final boolean isDestructuringAllowed) {

		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		if (!iterator.hasNext()) {
			return new OptionalParseResult(null, optionalBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new OptionalParseResult(currentElement, optionalBindings);
			}
			if (!iterator.hasNext() && isDotted) {
				return new OptionalParseResult(currentElement, optionalBindings);
			}

			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

				binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent, isSuppliedPSpecial);

				final OptionalBinding optionalBinding = new OptionalBinding(currentParam, null, NullStruct.INSTANCE, isSpecial, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaListBindings destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct<?> customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

						final OptionalBinding optionalBinding = new OptionalBinding(varNameCurrent, destructuringForm, NullStruct.INSTANCE, false, suppliedPBinding);
						optionalBindings.add(optionalBinding);
					} else {
						final String printedElement = printer.print(currentParam);
						throw new ProgramErrorException("LambdaList &optional parameters must have between 1 and 3 parameters: " + printedElement);
					}
				} else {
					final LispStruct firstInCurrent = currentParam.getFirst();
					final LispStruct secondInCurrent = currentParam.getRest().getFirst();
					final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

					final SymbolStruct<?> varNameCurrent;
					DestructuringLambdaListBindings destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
					} else {
						if (isDestructuringAllowed) {
							if (firstInCurrent instanceof ListStruct) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								final ListStruct destructuringFormList = (ListStruct) firstInCurrent;
								destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								final String printedElement = printer.print(firstInCurrent);
								throw new ProgramErrorException("LambdaList &optional var name parameter must be a symbol or a destructuring list: " + printedElement);
							}
						} else {
							final String printedElement = printer.print(firstInCurrent);
							throw new ProgramErrorException("LambdaList &optional var name parameters must be a symbol: " + printedElement);
						}
					}

					LispStruct initForm = NullStruct.INSTANCE;
					if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
						initForm = secondInCurrent;
					}

					final LispStruct parameterValueInitForm = formAnalyzer.analyze(initForm, environment);

					LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
					int newBindingsPosition = currentLambda.getNextParameterNumber();
					environment.setBindingsPosition(newBindingsPosition);

					final boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

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
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						newBindingsPosition = currentLambda.getNextParameterNumber();
						environment.setBindingsPosition(newBindingsPosition);

						final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

						binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							final String printedElement = printer.print(thirdInCurrent);
							throw new ProgramErrorException("LambdaList &optional supplied-p parameters must be a symbol: " + printedElement);
						}

						final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						currentLambda = Environments.getEnclosingLambda(environment);
						newBindingsPosition = currentLambda.getNextParameterNumber();
						environment.setBindingsPosition(newBindingsPosition);

						final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

						binding = new EnvironmentParameterBinding(suppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, isSuppliedPSpecial);
					}

					final OptionalBinding optionalBinding = new OptionalBinding(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial, suppliedPBinding);
					optionalBindings.add(optionalBinding);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &optional parameters must be a symbol or a list: " + printedElement);
			}
		} while (iterator.hasNext());

		return new OptionalParseResult(currentElement, optionalBindings);
	}

	protected RestParseResult parseRestBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                           final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &rest parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();

		final SymbolStruct<?> currentParam;
		DestructuringLambdaListBindings destructuringForm = null;
		if (currentElement instanceof SymbolStruct) {
			currentParam = (SymbolStruct) currentElement;
		} else {
			if (isDestructuringAllowed) {
				if (currentElement instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) currentElement;
					destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					final String printedElement = printer.print(currentElement);
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + printedElement);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + printedElement);
			}
		}

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

		final RestBinding restBinding = new RestBinding(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(currentElement, restBinding);
	}

	protected RestParseResult parseDottedRestBinding(final Environment environment, final LispStruct dottedRest,
	                                                 final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		final SymbolStruct<?> currentParam;
		DestructuringLambdaListBindings destructuringForm = null;
		if (dottedRest instanceof SymbolStruct) {
			currentParam = (SymbolStruct) dottedRest;
		} else {
			if (isDestructuringAllowed) {
				if (dottedRest instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) dottedRest;
					destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					final String printedElement = printer.print(dottedRest);
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + printedElement);
				}
			} else {
				final String printedElement = printer.print(dottedRest);
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + printedElement);
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

		final RestBinding restBinding = new RestBinding(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(dottedRest, restBinding);
	}

	protected BodyParseResult parseBodyBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                           final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &body parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();

		final SymbolStruct<?> currentParam;
		DestructuringLambdaListBindings destructuringForm = null;
		if (currentElement instanceof SymbolStruct) {
			currentParam = (SymbolStruct) currentElement;
		} else {
			if (isDestructuringAllowed) {
				if (currentElement instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) currentElement;
					destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					final String printedElement = printer.print(currentElement);
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + printedElement);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + printedElement);
			}
		}

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

		final BodyBinding bodyBinding = new BodyBinding(currentParam, destructuringForm, isSpecial);
		return new BodyParseResult(currentElement, bodyBinding);
	}

	protected KeyParseResult parseKeyBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                          final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		final List<KeyBinding> keyBindings = new ArrayList<>();

		if (!iterator.hasNext()) {
			return new KeyParseResult(null, keyBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new KeyParseResult(currentElement, keyBindings);
			}

			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct) currentElement;
				final KeywordStruct keyName = getKeywordStruct(currentParam.getName());

				final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
				int newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSpecial = Environments.isSpecial(declareElement, currentParam);

				EnvironmentParameterBinding binding = new EnvironmentParameterBinding(currentParam, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				newBindingsPosition = currentLambda.getNextParameterNumber();
				environment.setBindingsPosition(newBindingsPosition);

				final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

				binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent, isSuppliedPSpecial);

				final KeyBinding keyBinding = new KeyBinding(currentParam, null, NullStruct.INSTANCE, isSpecial, keyName, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final SymbolStruct<?> varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaListBindings destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct<?> customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent);

						final KeyBinding keyBinding = new KeyBinding(varNameCurrent, destructuringForm, NullStruct.INSTANCE, false, varKeyNameCurrent, suppliedPBinding);
						keyBindings.add(keyBinding);
					} else {
						final String printedElement = printer.print(currentParam);
						throw new ProgramErrorException("LambdaList &key parameters must have between 1 and 3 parameters: " + printedElement);
					}
				} else {
					final LispStruct firstInCurrent = currentParam.getFirst();
					final LispStruct secondInCurrent = currentParam.getRest().getFirst();
					final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

					final SymbolStruct<?> varNameCurrent;
					final SymbolStruct<?> varKeyNameCurrent;
					DestructuringLambdaListBindings destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
						varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
					} else if (firstInCurrent instanceof ListStruct) {
						final ListStruct currentVar = (ListStruct) firstInCurrent;
						if (currentVar.size() == 2) {
							final LispStruct firstInCurrentVar = currentVar.getFirst();
							if (firstInCurrentVar instanceof SymbolStruct) {
								varKeyNameCurrent = (SymbolStruct) firstInCurrentVar;
							} else {
								final String printedElement = printer.print(firstInCurrentVar);
								throw new ProgramErrorException("LambdaList &key var name list key-name parameters must be a symbol: " + printedElement);
							}

							final LispStruct secondInCurrentVar = currentVar.getRest().getFirst();
							if (!(secondInCurrentVar instanceof SymbolStruct)) {
								final String printedElement = printer.print(secondInCurrentVar);
								throw new ProgramErrorException("LambdaList &key var name list name parameters must be a symbol: " + printedElement);
							}
							varNameCurrent = (SymbolStruct) secondInCurrentVar;
						} else {
							if (isDestructuringAllowed) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
								final ListStruct destructuringFormList = (ListStruct) currentElement;
								destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								final String printedElement = printer.print(currentVar);
								throw new ProgramErrorException("LambdaList &key var name list parameters must have 2 parameters: " + printedElement);
							}
						}
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

					final boolean isSpecial = Environments.isSpecial(declareElement, varNameCurrent);

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
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						newBindingsPosition = currentLambda.getNextParameterNumber();
						environment.setBindingsPosition(newBindingsPosition);

						final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, customSuppliedPCurrent);

						binding = new EnvironmentParameterBinding(customSuppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPBinding(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							final String printedElement = printer.print(thirdInCurrent);
							throw new ProgramErrorException("LambdaList &key supplied-p parameters must be a symbol: " + printedElement);
						}

						final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						currentLambda = Environments.getEnclosingLambda(environment);
						newBindingsPosition = currentLambda.getNextParameterNumber();
						environment.setBindingsPosition(newBindingsPosition);

						final boolean isSuppliedPSpecial = Environments.isSpecial(declareElement, suppliedPCurrent);

						binding = new EnvironmentParameterBinding(suppliedPCurrent, TType.INSTANCE, NullStruct.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, isSuppliedPSpecial);
					}

					final KeyBinding keyBinding = new KeyBinding(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial, varKeyNameCurrent, suppliedPBinding);
					keyBindings.add(keyBinding);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &key parameters must be a symbol or a list: " + printedElement);
			}

		} while (iterator.hasNext());

		return new KeyParseResult(currentElement, keyBindings);
	}

	protected AuxParseResult parseAuxBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                          final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		final List<AuxBinding> auxBindings = new ArrayList<>();

		if (!iterator.hasNext()) {
			return new AuxParseResult(null, auxBindings);
		}

		LispStruct currentElement;
		do {
			currentElement = iterator.next();
			if (isLambdaListKeyword(currentElement)) {
				return new AuxParseResult(currentElement, auxBindings);
			}

			if (currentElement instanceof SymbolStruct) {
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

				final AuxBinding auxBinding = new AuxBinding(currentParam, null, NullStruct.INSTANCE, isSpecial);
				auxBindings.add(auxBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 2)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaListBindings destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final AuxBinding auxBinding = new AuxBinding(varNameCurrent, destructuringForm, NullStruct.INSTANCE);
						auxBindings.add(auxBinding);
					} else {
						final String printedElement = printer.print(currentParam);
						throw new ProgramErrorException("LambdaList &aux parameters must have between 1 and 2 parameters: " + printedElement);
					}
				} else {
					final LispStruct firstInCurrent = currentParam.getFirst();
					final LispStruct secondInCurrent = currentParam.getRest().getFirst();

					final SymbolStruct<?> varNameCurrent;
					DestructuringLambdaListBindings destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
					} else {
						if (isDestructuringAllowed) {
							if (firstInCurrent instanceof ListStruct) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								final ListStruct destructuringFormList = (ListStruct) firstInCurrent;
								destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								final String printedElement = printer.print(firstInCurrent);
								throw new ProgramErrorException("LambdaList &aux var name parameter must be a symbol or a destructuring list: " + printedElement);
							}
						} else {
							final String printedElement = printer.print(firstInCurrent);
							throw new ProgramErrorException("LambdaList &aux var name parameters must be a symbol: " + printedElement);
						}
					}

					LispStruct initForm = NullStruct.INSTANCE;
					if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
						initForm = secondInCurrent;
					}

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

					final AuxBinding auxBinding = new AuxBinding(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial);
					auxBindings.add(auxBinding);
				}
			} else {
				final String printedElement = printer.print(currentElement);
				throw new ProgramErrorException("LambdaList &aux parameters must be a symbol or a list: " + printedElement);
			}
		} while (iterator.hasNext());

		return new AuxParseResult(currentElement, auxBindings);
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
