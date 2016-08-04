package jcl.lang.statics;

import java.util.HashSet;
import java.util.Set;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.VariableStruct;
import jcl.lang.condition.exception.TypeErrorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class PrintCaseVariable extends VariableStruct<KeywordStruct> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final Set<KeywordStruct> CASE_KEYWORDS = new HashSet<>();

	private static final KeywordStruct UPCASE_KEYWORD = KeywordStruct.valueOf("UPCASE");
	private static final KeywordStruct DOWNCASE_KEYWORD = KeywordStruct.valueOf("DOWNCASE");
	private static final KeywordStruct CAPITALIZE_KEYWORD = KeywordStruct.valueOf("CAPITALIZE");


	static {
		CASE_KEYWORDS.add(UPCASE_KEYWORD);
		CASE_KEYWORDS.add(DOWNCASE_KEYWORD);
		CASE_KEYWORDS.add(CAPITALIZE_KEYWORD);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintCaseVariable.class);

	private PrintCaseVariable() {
		super("*PRINT-CASE*", GlobalPackageStruct.COMMON_LISP, UPCASE_KEYWORD);
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

			super.setValue(UPCASE_KEYWORD);
		}
	}
}
