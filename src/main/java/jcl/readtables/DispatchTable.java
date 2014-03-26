package jcl.readtables;

import jcl.LispStruct;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.macrofunction.ReaderMacroFunction;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.syntax.reader.ReadResult;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class DispatchTable extends ReaderMacroFunction {

	private final Map<Integer, ReaderMacroFunction> macroFunctionMap = new ConcurrentHashMap<>();

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {

		final ReadResult readResult = reader.readChar(false, null, false);
		if (readResult.wasEOF()) {
			throw new ReaderErrorException("No read macro function for character " + readResult + '.');
		}

		final int readChar = readResult.getResult();
		final ReaderMacroFunction macroFn = getMacroCharacter(readChar);
		if (macroFn == null) {
			throw new ReaderErrorException("No read macro function for character " + readChar + '.');
		}

		return macroFn.readMacro(readChar, reader, numArg);
	}

	public ReaderMacroFunction getMacroCharacter(final int codePoint) {
		return macroFunctionMap.get(codePoint);
	}

	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction) {
		macroFunctionMap.put(codePoint, readerMacroFunction);
	}
}
