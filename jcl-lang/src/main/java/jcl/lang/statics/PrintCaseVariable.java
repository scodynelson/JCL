package jcl.lang.statics;

import java.util.HashSet;
import java.util.Set;

import jcl.lang.KeywordStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.condition.exception.TypeErrorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class PrintCaseVariable extends VariableStructImpl<KeywordStructImpl> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final Set<KeywordStructImpl> CASE_KEYWORDS = new HashSet<>();

	private static final KeywordStructImpl UPCASE_KEYWORD = KeywordStructImpl.valueOf("UPCASE");
	private static final KeywordStructImpl DOWNCASE_KEYWORD = KeywordStructImpl.valueOf("DOWNCASE");
	private static final KeywordStructImpl CAPITALIZE_KEYWORD = KeywordStructImpl.valueOf("CAPITALIZE");


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
		if (!(value instanceof KeywordStructImpl)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Keyword value.");
		}
		final KeywordStructImpl variableValue = (KeywordStructImpl) value;

		if (CASE_KEYWORDS.contains(variableValue)) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: *PRINT-CASE* had illegal value {}. Reset to :UPCASE", variableValue);

			super.setValue(UPCASE_KEYWORD);
		}
	}
}
