package jcl.system;

import jcl.lang.SynonymStreamStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.internal.stream.EmptyStreamStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;

final class BootstrapSymbols {

	static void bootstrap() {
		GlobalPackageStruct.COMMON_LISP.export(CommonLispSymbols.SETF);
	}

	static void bootstrapStreams() {
		final TwoWayStreamStruct terminalIo = TwoWayStreamStruct.toTwoWayStream(
				EmptyStreamStructImpl.INSTANCE, EmptyStreamStructImpl.INSTANCE
		);
		terminalIo.setInteractive(true);
		CommonLispSymbols.TERMINAL_IO.setValue(terminalIo);

		CommonLispSymbols.DEBUG_IO.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.ERROR_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.QUERY_IO.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.STANDARD_INPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.STANDARD_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
		CommonLispSymbols.TRACE_OUTPUT.setValue(SynonymStreamStruct.toSynonymStream(CommonLispSymbols.TERMINAL_IO));
	}
}
