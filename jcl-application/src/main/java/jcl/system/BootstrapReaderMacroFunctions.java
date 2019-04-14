package jcl.system;

import jcl.functions.reader.ApostropheReaderMacroFunction;
import jcl.functions.reader.AtSignReaderMacroFunction;
import jcl.functions.reader.BackquoteReaderMacroFunction;
import jcl.functions.reader.CommaReaderMacroFunction;
import jcl.functions.reader.DollarSignReaderMacroFunction;
import jcl.functions.reader.LeftParenthesisReaderMacroFunction;
import jcl.functions.reader.QuotationMarkReaderMacroFunction;
import jcl.functions.reader.ReaderMacroFunctionImpl;
import jcl.functions.reader.RightParenthesisReaderMacroFunction;
import jcl.functions.reader.SemicolonReaderMacroFunction;
import jcl.functions.reader.SharpAReaderMacroFunction;
import jcl.functions.reader.SharpApostropheReaderMacroFunction;
import jcl.functions.reader.SharpAsteriskReaderMacroFunction;
import jcl.functions.reader.SharpBReaderMacroFunction;
import jcl.functions.reader.SharpBackslashReaderMacroFunction;
import jcl.functions.reader.SharpCReaderMacroFunction;
import jcl.functions.reader.SharpColonReaderMacroFunction;
import jcl.functions.reader.SharpEqualsSignReaderMacroFunction;
import jcl.functions.reader.SharpFullStopReaderMacroFunction;
import jcl.functions.reader.SharpHyphenMinusReaderMacroFunction;
import jcl.functions.reader.SharpIllegalReaderMacroFunction;
import jcl.functions.reader.SharpLeftParenthesisReaderMacroFunction;
import jcl.functions.reader.SharpOReaderMacroFunction;
import jcl.functions.reader.SharpPReaderMacroFunction;
import jcl.functions.reader.SharpPlusSignReaderMacroFunction;
import jcl.functions.reader.SharpRReaderMacroFunction;
import jcl.functions.reader.SharpSReaderMacroFunction;
import jcl.functions.reader.SharpSharpReaderMacroFunction;
import jcl.functions.reader.SharpUReaderMacroFunction;
import jcl.functions.reader.SharpVerticalBarReaderMacroFunction;
import jcl.functions.reader.SharpXReaderMacroFunction;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapReaderMacroFunctions {

	public static void bootstrap() throws Exception {
		ReaderMacroFunctionImpl readerMacroFunction = new ApostropheReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new AtSignReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new BackquoteReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new CommaReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new DollarSignReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new LeftParenthesisReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new QuotationMarkReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new RightParenthesisReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SemicolonReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpApostropheReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpAReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpAsteriskReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpBackslashReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpBReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpColonReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpCReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpEqualsSignReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpFullStopReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpHyphenMinusReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpIllegalReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpLeftParenthesisReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpOReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpPlusSignReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpPReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpRReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpSharpReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpSReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpUReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpVerticalBarReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();

		readerMacroFunction = new SharpXReaderMacroFunction();
		readerMacroFunction.afterPropertiesSet();
	}
}
