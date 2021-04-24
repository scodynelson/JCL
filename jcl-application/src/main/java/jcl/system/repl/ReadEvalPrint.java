package jcl.system.repl;

import jcl.compiler.function.InternalEval;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ConditionException;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.printer.Printer;
import jcl.reader.InternalRead;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class ReadEvalPrint {

	public static void funcall() {
		try {
			CommonLispSymbols.DASH.setValue(NILStruct.INSTANCE);

			CommonLispSymbols.PLUS.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.PLUS_PLUS.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.PLUS_PLUS_PLUS.setValue(NILStruct.INSTANCE);

			CommonLispSymbols.SLASH.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.SLASH_SLASH.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.SLASH_SLASH_SLASH.setValue(NILStruct.INSTANCE);

			CommonLispSymbols.STAR.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.STAR_STAR.setValue(NILStruct.INSTANCE);
			CommonLispSymbols.STAR_STAR_STAR.setValue(NILStruct.INSTANCE);

			final InputStreamStruct inputStreamStruct = (InputStreamStruct) CommonLispSymbols.STANDARD_INPUT.getValue();

			int counter = 1;
			while (true) {
				try {
					// PROMPT --------------
					final PackageStruct currentPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();
					final String currentPackageName = currentPackage.getName();
					log.info("{}: {}> ", currentPackageName, counter++);

					// READ --------------
					final LispStruct whatRead = InternalRead.read(inputStreamStruct, true, NILStruct.INSTANCE, false);

					// bind '-' to the form just read
					CommonLispSymbols.DASH.setValue(whatRead);

					// EVAL --------------
					final LispStruct value = InternalEval.eval(whatRead);

					// bind '**' and '***' to their appropriate values
					CommonLispSymbols.STAR_STAR_STAR.setValue(CommonLispSymbols.STAR_STAR.getValue());
					CommonLispSymbols.STAR_STAR.setValue(CommonLispSymbols.STAR.getValue());

					// bind '//' and '///' to their appropriate values
					CommonLispSymbols.SLASH_SLASH_SLASH.setValue(CommonLispSymbols.SLASH_SLASH.getValue());
					CommonLispSymbols.SLASH_SLASH.setValue(CommonLispSymbols.SLASH.getValue());

					// bind '*' and '/' values to the form just evaluated
					if (value instanceof ValuesStruct) {
						final ValuesStruct values = (ValuesStruct) value;
						CommonLispSymbols.STAR.setValue(values.getPrimaryValue());
						CommonLispSymbols.SLASH.setValue(ListStruct.toLispList(values.getValuesList()));
					} else {
						CommonLispSymbols.STAR.setValue(value);
						// null check
						CommonLispSymbols.SLASH.setValue(ListStruct.toLispList(value));
					}

					// bind '+' to the form just evaluated and '++' and '+++' to their appropriate values
					CommonLispSymbols.PLUS_PLUS_PLUS.setValue(CommonLispSymbols.PLUS_PLUS.getValue());
					CommonLispSymbols.PLUS_PLUS.setValue(CommonLispSymbols.PLUS.getValue());
					CommonLispSymbols.PLUS.setValue(CommonLispSymbols.DASH.getValue());

					if (value == null) {
						log.warn("Setting * to NIL since it had no value.");
						CommonLispSymbols.STAR.setValue(NILStruct.INSTANCE);
					}

					// PRINT -------------
					if (value == null) {
						log.info("; -- No Value --");
					} else {
						final String printedValue = Printer.print(value);
						log.info(printedValue);
					}
				} catch (final ReaderErrorException ex) {

					// Consume the rest of the input so we don't attempt to parse the rest of the input when an error occurs.
					Integer readChar;
					do {
						final ReadCharResult readResult = inputStreamStruct.readChar(false, null);
						readChar = readResult.getResult();
					} while ((readChar != null) && (readChar != -1) && (readChar != 10));

					log.warn("; WARNING: Reader Exception condition during Read -> ", ex);
				} catch (final ConditionException ex) {
					log.warn("; WARNING: Condition Exception condition during Read -> ", ex);
				} catch (final RuntimeException ex) {
					log.error("; WARNING: Runtime Exception condition -> ", ex);
				} catch (final StackOverflowError err) {
					log.error("; Stack Overflow Error, restarting REP function.", err);
				} catch (final VerifyError err) {
					log.error("; ERROR: loading class, {}", err.getMessage(), err);
				} finally {
//					CommonLispSymbols.DASH.unbindDynamicValue();
				}
			}
		} finally {
//			unbindREPLVariable(CommonLispSymbols.STAR);
//			unbindREPLVariable(CommonLispSymbols.STAR_STAR);
//			unbindREPLVariable(CommonLispSymbols.STAR_STAR_STAR);
//
//			unbindREPLVariable(CommonLispSymbols.SLASH);
//			unbindREPLVariable(CommonLispSymbols.SLASH_SLASH);
//			unbindREPLVariable(CommonLispSymbols.SLASH_SLASH_SLASH);
//
//			unbindREPLVariable(CommonLispSymbols.PLUS);
//			unbindREPLVariable(CommonLispSymbols.PLUS_PLUS);
//			unbindREPLVariable(CommonLispSymbols.PLUS_PLUS_PLUS);
//
//			unbindREPLVariable(CommonLispSymbols.DASH);
		}
	}

//	private void unbindREPLVariable(final VariableStructImpl<?> replVariable) {
//		if (replVariable.hasValue()) {
//			replVariable.unbindDynamicValue();
//		}
//	}
}
