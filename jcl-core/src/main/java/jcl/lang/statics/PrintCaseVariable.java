package jcl.lang.statics;

import java.util.HashSet;
import java.util.Set;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import lombok.extern.log4j.Log4j2;

@Log4j2
final class PrintCaseVariable extends VariableStructImpl<KeywordStruct> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final Set<KeywordStruct> CASE_KEYWORDS = new HashSet<>();

	private static final KeywordStruct UPCASE_KEYWORD = KeywordStruct.toLispKeyword("UPCASE");
	private static final KeywordStruct DOWNCASE_KEYWORD = KeywordStruct.toLispKeyword("DOWNCASE");
	private static final KeywordStruct CAPITALIZE_KEYWORD = KeywordStruct.toLispKeyword("CAPITALIZE");


	static {
		CASE_KEYWORDS.add(UPCASE_KEYWORD);
		CASE_KEYWORDS.add(DOWNCASE_KEYWORD);
		CASE_KEYWORDS.add(CAPITALIZE_KEYWORD);
	}

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
			log.warn("Error: *PRINT-CASE* had illegal value {}. Reset to :UPCASE", variableValue);

			super.setValue(UPCASE_KEYWORD);
		}
	}
}
