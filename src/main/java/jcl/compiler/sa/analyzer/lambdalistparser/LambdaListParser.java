package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import jcl.LispStruct;
import jcl.compiler.CompilerConstants;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.DestructuringLambdaList;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
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

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, TType.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final WholeParameter wholeBinding = new WholeParameter(currentParam, isSpecial);
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

		final Binding binding = new Binding(currentParam, TType.INSTANCE);
		environment.addDynamicBinding(binding);

		final EnvironmentParameter environmentBinding = new EnvironmentParameter(currentParam);
		return new EnvironmentParseResult(currentElement, environmentBinding);
	}

	protected RequiredParseResult parseRequiredBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final DeclareStruct declareElement, final boolean isDotted,
	                                                    final boolean isDestructuringAllowed) {

		final List<RequiredParameter> requiredBindings = new ArrayList<>();

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
			DestructuringLambdaList destructuringForm = null;
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

			final boolean isSpecial = declareElement.getSpecialDeclarations()
			                                        .stream()
			                                        .map(SpecialDeclarationStruct::getVar)
			                                        .anyMatch(Predicate.isEqual(currentParam));

			final Binding binding = new Binding(currentParam, TType.INSTANCE);
			if (isSpecial) {
				environment.addDynamicBinding(binding);
			} else {
				environment.addLexicalBinding(binding);
			}

			final RequiredParameter requiredBinding = new RequiredParameter(currentParam, destructuringForm, isSpecial);
			requiredBindings.add(requiredBinding);
		} while (iterator.hasNext());

		return new RequiredParseResult(currentElement, requiredBindings);
	}

	protected OptionalParseResult parseOptionalBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                                    final DeclareStruct declareElement, final boolean isDotted,
	                                                    final boolean isDestructuringAllowed) {

		final List<OptionalParameter> optionalBindings = new ArrayList<>();

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

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				Binding binding = new Binding(currentParam, TType.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
				                                                 .stream()
				                                                 .map(SpecialDeclarationStruct::getVar)
				                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

				binding = new Binding(customSuppliedPCurrent, TType.INSTANCE);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);

				final OptionalParameter optionalBinding = new OptionalParameter(currentParam, null, NullStruct.INSTANCE, isSpecial, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct<?> customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent);

						final OptionalParameter optionalBinding = new OptionalParameter(varNameCurrent, destructuringForm, NullStruct.INSTANCE, false, suppliedPBinding);
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
					DestructuringLambdaList destructuringForm = null;
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

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					Binding binding = new Binding(varNameCurrent, TType.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final SuppliedPParameter suppliedPBinding;
					if (thirdInCurrent.equals(NullStruct.INSTANCE)) {
						final String paramName = varNameCurrent.getName();
						final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

						binding = new Binding(customSuppliedPCurrent, TType.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							final String printedElement = printer.print(thirdInCurrent);
							throw new ProgramErrorException("LambdaList &optional supplied-p parameters must be a symbol: " + printedElement);
						}

						final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(suppliedPCurrent));

						binding = new Binding(suppliedPCurrent, TType.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(suppliedPCurrent, isSuppliedPSpecial);
					}

					final OptionalParameter optionalBinding = new OptionalParameter(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial, suppliedPBinding);
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
		DestructuringLambdaList destructuringForm = null;
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

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, TType.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestParameter restBinding = new RestParameter(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(currentElement, restBinding);
	}

	protected RestParseResult parseDottedRestBinding(final Environment environment, final LispStruct dottedRest,
	                                                 final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		final SymbolStruct<?> currentParam;
		DestructuringLambdaList destructuringForm = null;
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

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, TType.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestParameter restBinding = new RestParameter(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(dottedRest, restBinding);
	}

	protected BodyParseResult parseBodyBinding(final Environment environment, final Iterator<LispStruct> iterator,
	                                           final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &body parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();

		final SymbolStruct<?> currentParam;
		DestructuringLambdaList destructuringForm = null;
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

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, TType.INSTANCE);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final BodyParameter bodyBinding = new BodyParameter(currentParam, destructuringForm, isSpecial);
		return new BodyParseResult(currentElement, bodyBinding);
	}

	protected KeyParseResult parseKeyBindings(final Environment environment, final Iterator<LispStruct> iterator,
	                                          final DeclareStruct declareElement, final boolean isDestructuringAllowed) {

		final List<KeyParameter> keyBindings = new ArrayList<>();

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

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				Binding binding = new Binding(currentParam, TType.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
				                                                 .stream()
				                                                 .map(SpecialDeclarationStruct::getVar)
				                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

				binding = new Binding(customSuppliedPCurrent, TType.INSTANCE);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);

				final KeyParameter keyBinding = new KeyParameter(currentParam, null, NullStruct.INSTANCE, isSpecial, keyName, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final SymbolStruct<?> varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct<?> customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent);

						final KeyParameter keyBinding = new KeyParameter(varNameCurrent, destructuringForm, NullStruct.INSTANCE, false, varKeyNameCurrent, suppliedPBinding);
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
					DestructuringLambdaList destructuringForm = null;
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

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					Binding binding = new Binding(varNameCurrent, TType.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final SuppliedPParameter suppliedPBinding;
					if (thirdInCurrent.equals(NullStruct.INSTANCE)) {
						final String paramName = varNameCurrent.getName();
						final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct<?> customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

						binding = new Binding(customSuppliedPCurrent, TType.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							final String printedElement = printer.print(thirdInCurrent);
							throw new ProgramErrorException("LambdaList &key supplied-p parameters must be a symbol: " + printedElement);
						}

						final SymbolStruct<?> suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(suppliedPCurrent));

						binding = new Binding(suppliedPCurrent, TType.INSTANCE);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(suppliedPCurrent, isSuppliedPSpecial);
					}

					final KeyParameter keyBinding = new KeyParameter(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial, varKeyNameCurrent, suppliedPBinding);
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

		final List<AuxParameter> auxBindings = new ArrayList<>();

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

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				final Binding binding = new Binding(currentParam, TType.INSTANCE);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final AuxParameter auxBinding = new AuxParameter(currentParam, null, NullStruct.INSTANCE, isSpecial);
				auxBindings.add(auxBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 2)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct<?> varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = destructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final AuxParameter auxBinding = new AuxParameter(varNameCurrent, destructuringForm, NullStruct.INSTANCE);
						auxBindings.add(auxBinding);
					} else {
						final String printedElement = printer.print(currentParam);
						throw new ProgramErrorException("LambdaList &aux parameters must have between 1 and 2 parameters: " + printedElement);
					}
				} else {
					final LispStruct firstInCurrent = currentParam.getFirst();
					final LispStruct secondInCurrent = currentParam.getRest().getFirst();

					final SymbolStruct<?> varNameCurrent;
					DestructuringLambdaList destructuringForm = null;
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

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					final Binding binding = new Binding(varNameCurrent, TType.INSTANCE);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final AuxParameter auxBinding = new AuxParameter(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial);
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