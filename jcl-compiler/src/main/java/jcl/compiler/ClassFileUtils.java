package jcl.compiler;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.stream.JavaStreamStructImpl;
import jcl.lang.statics.StreamVariables;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.benf.cfr.reader.api.CfrDriver;
import org.benf.cfr.reader.api.ClassFileSource;
import org.benf.cfr.reader.api.OutputSinkFactory;
import org.benf.cfr.reader.bytecode.analysis.parse.utils.Pair;
import org.objectweb.asm.Type;

@Slf4j
@UtilityClass
public final class ClassFileUtils {

	/**
	 * The size of the temporary byte array used to read class input streams chunk by chunk.
	 */
	private static final int INPUT_STREAM_DATA_CHUNK_SIZE = 4096;

	public static LispStruct disassemble(final FunctionStruct function) {
		if (function == null) {
			throw new ErrorException("Undefined function.");
		}

		final Class<?> clazz = function.getClass();
		final ClassLoader classLoader = clazz.getClassLoader();
		final String className = Type.getInternalName(clazz);
		final String fullClassName = className + ".class";

		final byte[] classBytes = readBytes(classLoader, fullClassName);
		final ClassFileSource source = new DisassembledClassFileSource(classBytes);

		// TODO: Should be STANDARD_OUTPUT
		final JavaStreamStructImpl standardOutput
				= (JavaStreamStructImpl) StreamVariables.TERMINAL_IO.getVariableValue()
				                                                    .getOutputStreamStruct();

		// NOTE: Don't close this writer, as it's the standard output.
		final PrintWriter writer = standardOutput.getOutputStream();

		final OutputSinkFactory outputSink = new PrintWriterOutputSinkFactory(writer);

		final CfrDriver driver = new CfrDriver.Builder()
				.withClassFileSource(source)
				.withOutputSink(outputSink)
				.build();
		driver.analyse(Collections.singletonList(fullClassName));

		writer.flush();

		return NILStruct.INSTANCE;
	}

	private static byte[] readBytes(final ClassLoader classLoader, final String className) {
		try (final InputStream inputStream = classLoader.getResourceAsStream(className);
		     final ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {

			final byte[] data = new byte[INPUT_STREAM_DATA_CHUNK_SIZE];
			int bytesRead;
			while ((bytesRead = inputStream.read(data, 0, data.length)) != -1) {
				outputStream.write(data, 0, bytesRead);
			}
			outputStream.flush();
			return outputStream.toByteArray();
		} catch (final IOException ex) {
			log.error("Exception during disassemble of {}.", className, ex);
			return null;
		}
	}

	private static class DisassembledClassFileSource implements ClassFileSource {

		private final byte[] classBytes;

		DisassembledClassFileSource(final byte[] classBytes) {
			this.classBytes = classBytes;
		}

		@Override
		public void informAnalysisRelativePathDetail(final String usePath, final String specPath) {
		}

		@Override
		public Collection<String> addJar(final String jarPath) {
			return null;
		}

		@Override
		public String getPossiblyRenamedPath(final String path) {
			return path;
		}

		@Override
		public Pair<byte[], String> getClassFileContent(final String inputPath) {
			return Pair.make(classBytes, inputPath);
		}
	}

	private static class PrintWriterOutputSinkFactory implements OutputSinkFactory {

		private final PrintWriter writer;

		PrintWriterOutputSinkFactory(final PrintWriter writer) {
			this.writer = writer;
		}

		@Override
		public List<SinkClass> getSupportedSinks(final SinkType sinkType, final Collection<SinkClass> collection) {
			return Arrays.asList(SinkClass.values());
		}

		@Override
		public <T> Sink<T> getSink(final SinkType sinkType, final SinkClass sinkClass) {
			if ((sinkType == SinkType.JAVA) || (sinkType == SinkType.EXCEPTION)) {
				return t -> writer.write(t.toString());
			} else {
				return t -> {
				};
			}
		}
	}
}
