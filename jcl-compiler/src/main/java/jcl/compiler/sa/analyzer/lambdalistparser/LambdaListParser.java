package jcl.compiler.sa.analyzer.lambdalistparser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

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
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerConstants;
import jcl.lang.statics.GlobalPackageStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class LambdaListParser {

	public static WholeParseResult parseWholeBinding(final Environment environment,
	                                                 final Iterator<LispStruct> iterator,
	                                                 final DeclareStruct declareElement) {

		final LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			throw new ProgramErrorException("LambdaList &whole parameters must be a symbol: " + currentElement);
		}
		final SymbolStruct currentParam = (SymbolStruct) currentElement;

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final WholeParameter wholeBinding = new WholeParameter(currentParam, isSpecial);
		return new WholeParseResult(wholeBinding);
	}

	public static EnvironmentParseResult parseEnvironmentBinding(final Environment environment,
	                                                             final Iterator<LispStruct> iterator,
	                                                             final boolean isAfterRequired) {

		LispStruct currentElement = iterator.next();
		if (!(currentElement instanceof SymbolStruct)) {
			throw new ProgramErrorException("LambdaList &environment parameters must be a symbol: " + currentElement);
		}
		final SymbolStruct currentParam = (SymbolStruct) currentElement;

		if (iterator.hasNext() && isAfterRequired) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				throw new ProgramErrorException("LambdaList &environment parameter must only have 1 parameter: " + currentElement);
			}
		}

		final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
		environment.addDynamicBinding(binding);

		final EnvironmentParameter environmentBinding = new EnvironmentParameter(currentParam);
		return new EnvironmentParseResult(currentElement, environmentBinding);
	}

	public static RequiredParseResult parseRequiredBindings(final Environment environment,
	                                                        final Iterator<LispStruct> iterator,
	                                                        final DeclareStruct declareElement,
	                                                        final boolean isDotted,
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

			final SymbolStruct currentParam;
			DestructuringLambdaList destructuringForm = null;
			if (currentElement instanceof SymbolStruct) {
				currentParam = (SymbolStruct) currentElement;
			} else {
				if (isDestructuringAllowed) {
					if (currentElement instanceof ListStruct) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
					} else {
						throw new ProgramErrorException("LambdaList required parameter must be a symbol or a destructuring list: " + currentElement);
					}
				} else {
					throw new ProgramErrorException("LambdaList required parameters must be a symbol: " + currentElement);
				}
			}

			final boolean isSpecial = declareElement.getSpecialDeclarations()
			                                        .stream()
			                                        .map(SpecialDeclarationStruct::getVar)
			                                        .anyMatch(Predicate.isEqual(currentParam));

			final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
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

	public static OptionalParseResult parseOptionalBindings(final Environment environment,
	                                                        final Iterator<LispStruct> iterator,
	                                                        final DeclareStruct declareElement,
	                                                        final boolean isDotted,
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
				final SymbolStruct currentParam = (SymbolStruct) currentElement;

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				Binding binding = new Binding(currentParam, CommonLispSymbols.T);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
				                                                 .stream()
				                                                 .map(SpecialDeclarationStruct::getVar)
				                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

				binding = new Binding(customSuppliedPCurrent, CommonLispSymbols.T);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);

				final OptionalParameter optionalBinding = new OptionalParameter(currentParam, null, NILStruct.INSTANCE, isSpecial, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				final long currentParamLength = currentParam.length().toJavaPLong();
				if ((currentParamLength < 1) || (currentParamLength > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent);

						final OptionalParameter optionalBinding = new OptionalParameter(varNameCurrent, destructuringForm, NILStruct.INSTANCE, false, suppliedPBinding);
						optionalBindings.add(optionalBinding);
					} else {
						throw new ProgramErrorException("LambdaList &optional parameters must have between 1 and 3 parameters: " + currentParam);
					}
				} else {
					final Iterator<LispStruct> currentIterator = currentParam.iterator();

					final LispStruct firstInCurrent = currentIterator.next();
					final LispStruct secondInCurrent;
					if (currentIterator.hasNext()) {
						secondInCurrent = currentIterator.next();
					} else {
						secondInCurrent = NILStruct.INSTANCE;
					}
					final LispStruct thirdInCurrent;
					if (currentIterator.hasNext()) {
						thirdInCurrent = currentIterator.next();
					} else {
						thirdInCurrent = NILStruct.INSTANCE;
					}

					final SymbolStruct varNameCurrent;
					DestructuringLambdaList destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
					} else {
						if (isDestructuringAllowed) {
							if (firstInCurrent instanceof ListStruct) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								final ListStruct destructuringFormList = (ListStruct) firstInCurrent;
								destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								throw new ProgramErrorException("LambdaList &optional var name parameter must be a symbol or a destructuring list: " + firstInCurrent);
							}
						} else {
							throw new ProgramErrorException("LambdaList &optional var name parameters must be a symbol: " + firstInCurrent);
						}
					}

					LispStruct initForm = NILStruct.INSTANCE;
					if (!secondInCurrent.eq(NILStruct.INSTANCE)) {
						initForm = secondInCurrent;
					}

					final LispStruct parameterValueInitForm = FormAnalyzer.analyze(initForm, environment);

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					Binding binding = new Binding(varNameCurrent, CommonLispSymbols.T);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final SuppliedPParameter suppliedPBinding;
					if (thirdInCurrent.eq(NILStruct.INSTANCE)) {
						final String paramName = varNameCurrent.getName();
						final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

						binding = new Binding(customSuppliedPCurrent, CommonLispSymbols.T);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							throw new ProgramErrorException("LambdaList &optional supplied-p parameters must be a symbol: " + thirdInCurrent);
						}

						final SymbolStruct suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(suppliedPCurrent));

						binding = new Binding(suppliedPCurrent, CommonLispSymbols.T);
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
				throw new ProgramErrorException("LambdaList &optional parameters must be a symbol or a list: " + currentElement);
			}
		} while (iterator.hasNext());

		return new OptionalParseResult(currentElement, optionalBindings);
	}

	public static RestParseResult parseRestBinding(final Environment environment,
	                                               final Iterator<LispStruct> iterator,
	                                               final DeclareStruct declareElement,
	                                               final boolean isDestructuringAllowed) {

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &rest parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();

		final SymbolStruct currentParam;
		DestructuringLambdaList destructuringForm = null;
		if (currentElement instanceof SymbolStruct) {
			currentParam = (SymbolStruct) currentElement;
		} else {
			if (isDestructuringAllowed) {
				if (currentElement instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) currentElement;
					destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + currentElement);
				}
			} else {
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + currentElement);
			}
		}

		if (iterator.hasNext()) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				throw new ProgramErrorException("LambdaList &rest parameter must only have 1 parameter: " + currentElement);
			}
		}

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestParameter restBinding = new RestParameter(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(currentElement, restBinding);
	}

	public static RestParseResult parseDottedRestBinding(final Environment environment,
	                                                     final LispStruct dottedRest,
	                                                     final DeclareStruct declareElement,
	                                                     final boolean isDestructuringAllowed) {

		final SymbolStruct currentParam;
		DestructuringLambdaList destructuringForm = null;
		if (dottedRest instanceof SymbolStruct) {
			currentParam = (SymbolStruct) dottedRest;
		} else {
			if (isDestructuringAllowed) {
				if (dottedRest instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) dottedRest;
					destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + dottedRest);
				}
			} else {
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + dottedRest);
			}
		}

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final RestParameter restBinding = new RestParameter(currentParam, destructuringForm, isSpecial);
		return new RestParseResult(dottedRest, restBinding);
	}

	public static BodyParseResult parseBodyBinding(final Environment environment,
	                                               final Iterator<LispStruct> iterator,
	                                               final DeclareStruct declareElement,
	                                               final boolean isDestructuringAllowed) {

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LambdaList &body parameter must be provided.");
		}

		LispStruct currentElement = iterator.next();

		final SymbolStruct currentParam;
		DestructuringLambdaList destructuringForm = null;
		if (currentElement instanceof SymbolStruct) {
			currentParam = (SymbolStruct) currentElement;
		} else {
			if (isDestructuringAllowed) {
				if (currentElement instanceof ListStruct) {
					final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
					currentParam = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
					final ListStruct destructuringFormList = (ListStruct) currentElement;
					destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
				} else {
					throw new ProgramErrorException("LambdaList &rest parameters must be a symbol or a destructuring list: " + currentElement);
				}
			} else {
				throw new ProgramErrorException("LambdaList &rest parameters must be a symbol: " + currentElement);
			}
		}

		if (iterator.hasNext()) {
			currentElement = iterator.next();
			if (!isLambdaListKeyword(currentElement)) {
				throw new ProgramErrorException("LambdaList &body parameter must only have 1 parameter: " + currentElement);
			}
		}

		final boolean isSpecial = declareElement.getSpecialDeclarations()
		                                        .stream()
		                                        .map(SpecialDeclarationStruct::getVar)
		                                        .anyMatch(Predicate.isEqual(currentParam));

		final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
		if (isSpecial) {
			environment.addDynamicBinding(binding);
		} else {
			environment.addLexicalBinding(binding);
		}

		final BodyParameter bodyBinding = new BodyParameter(currentParam, destructuringForm, isSpecial);
		return new BodyParseResult(currentElement, bodyBinding);
	}

	public static KeyParseResult parseKeyBindings(final Environment environment,
	                                              final Iterator<LispStruct> iterator,
	                                              final DeclareStruct declareElement,
	                                              final boolean isDestructuringAllowed) {

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
				final SymbolStruct currentParam = (SymbolStruct) currentElement;
				final KeywordStruct keyName = getKeywordStruct(currentParam.getName());

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				Binding binding = new Binding(currentParam, CommonLispSymbols.T);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final String paramName = currentParam.getName();
				final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
				final PackageStruct currentParamPackage = currentParam.getSymbolPackage();

				final SymbolStruct customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

				final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
				                                                 .stream()
				                                                 .map(SpecialDeclarationStruct::getVar)
				                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

				binding = new Binding(customSuppliedPCurrent, CommonLispSymbols.T);
				if (isSuppliedPSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);

				final KeyParameter keyBinding = new KeyParameter(currentParam, null, NILStruct.INSTANCE, isSpecial, keyName, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				final long currentParamLength = currentParam.length().toJavaPLong();
				if ((currentParamLength < 1) || (currentParamLength > 3)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final SymbolStruct varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final String customSuppliedPName = destructuringName + "-P-" + System.nanoTime();
						final SymbolStruct customSuppliedPCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(customSuppliedPName).getSymbol();
						final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent);

						final KeyParameter keyBinding = new KeyParameter(varNameCurrent, destructuringForm, NILStruct.INSTANCE, false, varKeyNameCurrent, suppliedPBinding);
						keyBindings.add(keyBinding);
					} else {
						throw new ProgramErrorException("LambdaList &key parameters must have between 1 and 3 parameters: " + currentParam);
					}
				} else {
					final Iterator<LispStruct> currentIterator = currentParam.iterator();

					final LispStruct firstInCurrent = currentIterator.next();
					final LispStruct secondInCurrent;
					if (currentIterator.hasNext()) {
						secondInCurrent = currentIterator.next();
					} else {
						secondInCurrent = NILStruct.INSTANCE;
					}
					final LispStruct thirdInCurrent;
					if (currentIterator.hasNext()) {
						thirdInCurrent = currentIterator.next();
					} else {
						thirdInCurrent = NILStruct.INSTANCE;
					}

					final SymbolStruct varNameCurrent;
					final SymbolStruct varKeyNameCurrent;
					DestructuringLambdaList destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
						varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
					} else if (firstInCurrent instanceof ListStruct) {
						final ListStruct currentVar = (ListStruct) firstInCurrent;
						final long currentVarLength = currentVar.length().toJavaPLong();
						if (currentVarLength == 2) {
							final LispStruct firstInCurrentVar = currentVar.car();
							if (firstInCurrentVar instanceof SymbolStruct) {
								varKeyNameCurrent = (SymbolStruct) firstInCurrentVar;
							} else {
								throw new ProgramErrorException("LambdaList &key var name list key-name parameters must be a symbol: " + firstInCurrentVar);
							}

							final LispStruct secondInCurrentVar = ((ListStruct) currentVar.cdr()).car();
							if (!(secondInCurrentVar instanceof SymbolStruct)) {
								throw new ProgramErrorException("LambdaList &key var name list name parameters must be a symbol: " + secondInCurrentVar);
							}
							varNameCurrent = (SymbolStruct) secondInCurrentVar;
						} else {
							if (isDestructuringAllowed) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								varKeyNameCurrent = getKeywordStruct(varNameCurrent.getName());
								final ListStruct destructuringFormList = (ListStruct) currentElement;
								destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								throw new ProgramErrorException("LambdaList &key var name list parameters must have 2 parameters: " + currentVar);
							}
						}
					} else {
						throw new ProgramErrorException("LambdaList &key var name parameters must be a symbol or a list: " + firstInCurrent);
					}

					LispStruct initForm = NILStruct.INSTANCE;
					if (!secondInCurrent.eq(NILStruct.INSTANCE)) {
						initForm = secondInCurrent;
					}

					final LispStruct parameterValueInitForm = FormAnalyzer.analyze(initForm, environment);

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					Binding binding = new Binding(varNameCurrent, CommonLispSymbols.T);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final SuppliedPParameter suppliedPBinding;
					if (thirdInCurrent.eq(NILStruct.INSTANCE)) {
						final String paramName = varNameCurrent.getName();
						final String customSuppliedPName = paramName + "-P-" + System.nanoTime();
						final PackageStruct currentParamPackage = varNameCurrent.getSymbolPackage();

						final SymbolStruct customSuppliedPCurrent = currentParamPackage.intern(customSuppliedPName).getSymbol();

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(customSuppliedPCurrent));

						binding = new Binding(customSuppliedPCurrent, CommonLispSymbols.T);
						if (isSuppliedPSpecial) {
							environment.addDynamicBinding(binding);
						} else {
							environment.addLexicalBinding(binding);
						}

						suppliedPBinding = new SuppliedPParameter(customSuppliedPCurrent, isSuppliedPSpecial);
					} else {
						if (!(thirdInCurrent instanceof SymbolStruct)) {
							throw new ProgramErrorException("LambdaList &key supplied-p parameters must be a symbol: " + thirdInCurrent);
						}

						final SymbolStruct suppliedPCurrent = (SymbolStruct) thirdInCurrent;

						final boolean isSuppliedPSpecial = declareElement.getSpecialDeclarations()
						                                                 .stream()
						                                                 .map(SpecialDeclarationStruct::getVar)
						                                                 .anyMatch(Predicate.isEqual(suppliedPCurrent));

						binding = new Binding(suppliedPCurrent, CommonLispSymbols.T);
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
				throw new ProgramErrorException("LambdaList &key parameters must be a symbol or a list: " + currentElement);
			}

		} while (iterator.hasNext());

		return new KeyParseResult(currentElement, keyBindings);
	}

	public static AuxParseResult parseAuxBindings(final Environment environment,
	                                              final Iterator<LispStruct> iterator,
	                                              final DeclareStruct declareElement,
	                                              final boolean isDestructuringAllowed) {

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
				final SymbolStruct currentParam = (SymbolStruct) currentElement;

				final boolean isSpecial = declareElement.getSpecialDeclarations()
				                                        .stream()
				                                        .map(SpecialDeclarationStruct::getVar)
				                                        .anyMatch(Predicate.isEqual(currentParam));

				final Binding binding = new Binding(currentParam, CommonLispSymbols.T);
				if (isSpecial) {
					environment.addDynamicBinding(binding);
				} else {
					environment.addLexicalBinding(binding);
				}

				final AuxParameter auxBinding = new AuxParameter(currentParam, null, NILStruct.INSTANCE, isSpecial);
				auxBindings.add(auxBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				final long currentParamLength = currentParam.length().toJavaPLong();
				if ((currentParamLength < 1) || (currentParamLength > 2)) {
					if (isDestructuringAllowed) {
						final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
						final SymbolStruct varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
						final ListStruct destructuringFormList = (ListStruct) currentElement;
						final DestructuringLambdaList destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);

						final AuxParameter auxBinding = new AuxParameter(varNameCurrent, destructuringForm, NILStruct.INSTANCE);
						auxBindings.add(auxBinding);
					} else {
						throw new ProgramErrorException("LambdaList &aux parameters must have between 1 and 2 parameters: " + currentParam);
					}
				} else {
					final Iterator<LispStruct> currentIterator = currentParam.iterator();

					final LispStruct firstInCurrent = currentIterator.next();
					final LispStruct secondInCurrent;
					if (currentIterator.hasNext()) {
						secondInCurrent = currentIterator.next();
					} else {
						secondInCurrent = NILStruct.INSTANCE;
					}

					final SymbolStruct varNameCurrent;
					DestructuringLambdaList destructuringForm = null;
					if (firstInCurrent instanceof SymbolStruct) {
						varNameCurrent = (SymbolStruct) firstInCurrent;
					} else {
						if (isDestructuringAllowed) {
							if (firstInCurrent instanceof ListStruct) {
								final String destructuringName = "DestructuringSymbolName-" + System.nanoTime();
								varNameCurrent = GlobalPackageStruct.COMMON_LISP_USER.intern(destructuringName).getSymbol();
								final ListStruct destructuringFormList = (ListStruct) firstInCurrent;
								destructuringForm = DestructuringLambdaListParser.parseDestructuringLambdaList(environment, destructuringFormList, declareElement);
							} else {
								throw new ProgramErrorException("LambdaList &aux var name parameter must be a symbol or a destructuring list: " + firstInCurrent);
							}
						} else {
							throw new ProgramErrorException("LambdaList &aux var name parameters must be a symbol: " + firstInCurrent);
						}
					}

					LispStruct initForm = NILStruct.INSTANCE;
					if (!secondInCurrent.eq(NILStruct.INSTANCE)) {
						initForm = secondInCurrent;
					}

					final LispStruct parameterValueInitForm = FormAnalyzer.analyze(initForm, environment);

					final boolean isSpecial = declareElement.getSpecialDeclarations()
					                                        .stream()
					                                        .map(SpecialDeclarationStruct::getVar)
					                                        .anyMatch(Predicate.isEqual(varNameCurrent));

					final Binding binding = new Binding(varNameCurrent, CommonLispSymbols.T);
					if (isSpecial) {
						environment.addDynamicBinding(binding);
					} else {
						environment.addLexicalBinding(binding);
					}

					final AuxParameter auxBinding = new AuxParameter(varNameCurrent, destructuringForm, parameterValueInitForm, isSpecial);
					auxBindings.add(auxBinding);
				}
			} else {
				throw new ProgramErrorException("LambdaList &aux parameters must be a symbol or a list: " + currentElement);
			}
		} while (iterator.hasNext());

		return new AuxParseResult(currentElement, auxBindings);
	}

	private static boolean isLambdaListKeyword(final LispStruct lispStruct) {
		return lispStruct.eq(CompilerConstants.AUX)
				|| lispStruct.eq(CompilerConstants.ALLOW_OTHER_KEYS)
				|| lispStruct.eq(CompilerConstants.KEY)
				|| lispStruct.eq(CompilerConstants.OPTIONAL)
				|| lispStruct.eq(CompilerConstants.REST)
				|| lispStruct.eq(CompilerConstants.WHOLE)
				|| lispStruct.eq(CompilerConstants.ENVIRONMENT)
				|| lispStruct.eq(CompilerConstants.BODY);
	}

	private static KeywordStruct getKeywordStruct(final String symbolName) {

		final PackageSymbolStruct symbol = GlobalPackageStruct.KEYWORD.findSymbol(symbolName);
		if (symbol.notFound()) {
			return KeywordStruct.toLispKeyword(symbolName);
		}
		// NOTE: This should be a safe cast because we're finding the symbol in the Keyword Package and they are only
		//       this type of symbol.
		return (KeywordStruct) symbol.getSymbol();
	}
}
