package jcl.functions.environment;

import java.io.PrintWriter;

import com.strobel.decompiler.Decompiler;
import com.strobel.decompiler.PlainTextOutput;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.internal.stream.JavaStreamStructImpl;
import jcl.lang.statics.StreamVariables;
import lombok.extern.slf4j.Slf4j;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public final class DisassembleFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "DISASSEMBLE";
	private static final String FN_ARGUMENT = "FN";

	/**
	 * Public constructor passing the documentation string.
	 */
	public DisassembleFunction() {
		super("Returns true if object is of type keyword; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FN_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct functionDesignator = arguments.getRequiredArgument(FN_ARGUMENT);

		FunctionStruct function = null;
		if (functionDesignator instanceof SymbolStruct) {
			final SymbolStruct functionSymbol = (SymbolStruct) functionDesignator;
			if (functionSymbol.hasFunction()) {
				function = functionSymbol.getFunction();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getMacroFunctionExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getSymbolMacroExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getCompilerMacroFunctionExpander();
			}
		} else if (functionDesignator instanceof ListStruct) {
			final SymbolStruct functionSymbol
					= (SymbolStruct) ((ListStruct) ((ListStruct) functionDesignator).getCdr()).getCar();
			if (functionSymbol.hasFunction()) {
				function = functionSymbol.getFunction();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getMacroFunctionExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getSymbolMacroExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getCompilerMacroFunctionExpander();
			}
		} else if (functionDesignator instanceof FunctionStruct) {
			function = (FunctionStruct) functionDesignator;
		} else {
			throw new TypeErrorException("Unsupported Function Designator.");
		}

		if (function == null) {
			throw new ErrorException("Undefined function.");
		}

		final Class<?> clazz = function.getClass();
		final String className = Type.getInternalName(clazz);
		try {
			final JavaStreamStructImpl standardOutput
					= (JavaStreamStructImpl) StreamVariables.TERMINAL_IO.getVariableValue()
					                                                    .getOutputStreamStruct();

			final PrintWriter writer = standardOutput.getOutputStream();
			Decompiler.decompile(clazz.getName(), new PlainTextOutput(writer));
			writer.flush();


//			final TraceClassVisitor visitor = new TraceClassVisitor(standardOutput.getOutputStream());
//
//			final ClassLoader classLoader = clazz.getClassLoader();
//			final InputStream inputStream = classLoader.getResourceAsStream(className + ".class");
//
//			final ClassReader classReader = new ClassReader(inputStream);
////			classReader.accept(visitor, 0);
//			classReader.accept(visitor, ClassReader.EXPAND_FRAMES);
		} catch (final Exception ex) {
			log.error("Exception during disassemble of {}.", className, ex);
		}

		return NILStruct.INSTANCE;
	}
}
