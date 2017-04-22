package jcl.lang;

import jcl.lang.statics.PrinterVariables;
import org.junit.Assert;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test class utility methods.
 */
public final class ToStringTestUtils {

	/**
	 * Private constructor.
	 */
	private ToStringTestUtils() {
	}

	/**
	 * Validates the {@link Object#toString()} method, ensuring that escaping is off and reset after the testing.
	 *
	 * @param str
	 * 		the expected result {@link String} from the {@link Object#toString()} invocation
	 * @param struct
	 * 		the {@link Object} to perform the {@link Object#toString()} operation on
	 */
	public static <T extends LispStruct> void validateToStringWithNoEscapes(final String str, final T struct) {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue();
		try {
			PrinterVariables.PRINT_ESCAPE.setValue(NILStruct.INSTANCE);
			final String result = struct.toString();
			Assert.assertThat(result, is(str));
		} finally {
			PrinterVariables.PRINT_ESCAPE.setValue(printEscape);
		}
	}
}
