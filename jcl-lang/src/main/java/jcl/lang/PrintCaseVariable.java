package jcl.lang;

import java.util.HashSet;
import java.util.Set;

import jcl.lang.condition.exception.TypeErrorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class PrintCaseVariable extends VariableStruct<KeywordStruct> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final Set<KeywordStruct> CASE_KEYWORDS = new HashSet<>();

	static {
		CASE_KEYWORDS.add(CommonLispSymbols.UPCASE_KEYWORD);
		CASE_KEYWORDS.add(CommonLispSymbols.DOWNCASE_KEYWORD);
		CASE_KEYWORDS.add(CommonLispSymbols.CAPITALIZE_KEYWORD);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintCaseVariable.class);

	private PrintCaseVariable() {
		super("*PRINT-CASE*", GlobalPackageStruct.COMMON_LISP, CommonLispSymbols.UPCASE_KEYWORD);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof KeywordStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Keyword value.");
		}
		final KeywordStruct variableValue = (KeywordStruct) value;

		if (CASE_KEYWORDS.contains(variableValue)) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: *PRINT-CASE* had illegal value {}. Reset to :UPCASE", variableValue);

			super.setValue(CommonLispSymbols.UPCASE_KEYWORD);
		}
	}
}
