package jcl.system;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.nio.file.Path;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.specialoperator.lambda.NewLambdaCodeGenerator;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.streams.CharacterStreamStruct;
import jcl.streams.FileStreamStruct;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.symbols.SpecialOperator;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.CheckMethodAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class ReadEvalPrint {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadEvalPrint.class);

	@Autowired
	private ApplicationContext context;

	@Autowired
	private Printer printer;

	public Object funcall(final String... args) {
		Object temp = SpecialOperator.BLOCK;
		temp = CommonLispSymbols.ABORT;

		// get local references to the basic functions
//		Function1 eval = (Function1) CommonLispFunctions.StdFunctions.Eval;

		// The basic loop. It keeps looping until someone calls the
		// Lisp EXIT function.

		if (args.length == 1) {
			final String fileName = args[0];
			final Path path = new File(fileName).toPath();

			try {
				final InputStream fileStream = new FileStreamStruct(path);
				return doStuff(fileStream, true);
			} catch (final StreamErrorException ex) {
				LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
				return null;
			}
		} else {
			try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
				final InputStream characterStream = new CharacterStreamStruct(System.in, loggerOutputStream);
				return doStuff(characterStream, false);
			} catch (final IOException ex) {
				LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
				return null;
			}
//			catch (StreamErrorException ex) {
//				LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
//				return null;
//			}
		}
	}

	private Object doStuff(final InputStream inputStream, final boolean isFile) {

		// get local references to the basic functions
//		Function1 eval = (Function1) CommonLispFunctions.StdFunctions.Eval;

		// The basic loop. It keeps looping until someone calls the
		// Lisp EXIT function.

		try {
			final Reader reader = context.getBean(Reader.class, inputStream);

			// bind all of the interaction variables to their values
//			Variable.Star.bind(Variable.Star.getValue());
//			Variable.StarStar.bind(Variable.StarStar.getValue());
//			Variable.StarStarStar.bind(Variable.StarStarStar.getValue());
//
//			Variable.Plus.bind(Variable.Plus.getValue());
//			Variable.PlusPlus.bind(Variable.PlusPlus.getValue());
//			Variable.PlusPlusPlus.bind(Variable.PlusPlusPlus.getValue());
//
//			Variable.Slash.bind(Variable.Slash.getValue());
//			Variable.SlashSlash.bind(Variable.SlashSlash.getValue());
//			Variable.SlashSlashSlash.bind(Variable.SlashSlashSlash.getValue());
//
//			Variable.Dash.bind(null);

			// Start of the REP loop
			int lineCounter = 0;
			while (true) {
//				Object value = null;
				try {
					// THE PROMPT
					final PackageStruct pkg = PackageVariables.PACKAGE.getValue();
					LOGGER.info("{}: {}> ", pkg.getName(), ++lineCounter);

					// READ --------------

					LispStruct whatRead = null;
					try {
						if (isFile) {
							whatRead = reader.read(false, null, false);
						} else {
							whatRead = reader.read(true, NullStruct.INSTANCE, false);
						}
						if (whatRead != null) {
							LOGGER.debug("READ: {}", whatRead.getClass().getSimpleName());

							final String printedWhatRead = printer.print(whatRead);
							LOGGER.debug("{}", printedWhatRead);
						} else {
							LOGGER.warn("; WARNING: Null response from reader");
						}
					} catch (final ReaderErrorException ex) {

						// Consume the rest of the input so we don't attempt to parse the rest of the input when an error occurs.
						ReadPeekResult readResult = reader.readChar(false, null, true);
						Integer readChar = readResult.getResult();
						while ((readChar != null) && (readChar != -1) && (readChar != 10)) {
							readResult = reader.readChar(false, null, true);
							readChar = readResult.getResult();
						}

						LOGGER.warn("; WARNING: Reader Exception condition during Read -> ", ex);
					} catch (final Exception ex) {
						LOGGER.warn("; WARNING: Exception condition during Read -> ", ex);
						break;
					}

					// bind '-' to the form just read
//					Variable.Dash.bind(whatRead);

					// EVAL --------------
//					value = eval.funcall(whatRead);

					// TEMPORARY: ANALYZER

					if (whatRead != null) {
//						generatorTest();

						LispStruct whatAnalyzed = null;
						try {
//							final SemanticAnalyzer sa = context.getBean(SemanticAnalyzer.class);
//							whatAnalyzed = sa.analyze(whatRead);
//
//							if (whatAnalyzed != null) {
//								LOGGER.debug("ANALYZED:");
//								LOGGER.debug("{}", whatAnalyzed);
//							} else {
//								LOGGER.warn("; WARNING: Null response from analyzer");
//							}
						} catch (final ReaderErrorException ex) {
							LOGGER.warn("; WARNING: Analysis Exception condition during Analyzer operation -> ", ex);
						} catch (final Exception ex) {
							LOGGER.warn("; WARNING: Analysis condition during Analyzer operation -> ", ex);
							break;
						}
					}

					// save the form evaluated and its predecessors
//					Variable.PlusPlusPlus.setValue(Variable.PlusPlus.getValue());
//					Variable.PlusPlus.setValue(Variable.Plus.getValue());
//					Variable.Plus.setValue(whatRead);

					// Now save the previous returned values - if a value was returned
//					if (value != null) {
//						Variable.StarStarStar.setValue(Variable.StarStar.getValue());
//						Variable.StarStar.setValue(Variable.Star.getValue());
//
//						Variable.SlashSlashSlash.setValue(Variable.SlashSlash.getValue());
//						Variable.SlashSlash.setValue(Variable.Slash.getValue());
//
					// There is different treatment depending on whether there were
					// multiple values returned
//						if (value instanceof Object[]) {
//							Object[] mv = (Object[]) value;
//							if (mv.length == 0) {
//								Variable.Star.setValue(null);
//							} else {
//								Variable.Star.setValue(mv[0]);
//							}
//							Variable.Slash.setValue(lisp.common.type.List.Factory.newInstance(mv));
//						} else {
//							Variable.Star.setValue(value);
//							Variable.Slash.setValue(lisp.common.type.List.Factory.newInstance(value));
//						}
//					}

					// PRINT -------------
					if (whatRead == null) {
						LOGGER.info(";-- No Value --");
//					} else if (value instanceof Object[]) {
//						final Object[] mv = (Object[]) value;
//						int count = mv.length;
//						if (count == 0) {
//							LOGGER.info(";-- No Value --");
//						} else {
//							count--;
//							for (int index = 0; index < count; index++) {
//								final Object lcl = mv[index];
//								LOGGER.info(lcl.toString());
//								LOGGER.info(" ;");
//							}
//							LOGGER.info(mv[count].toString());
//						}
					} else {
						final String printedValue = printer.print(whatRead);
						LOGGER.info(printedValue);
					}
				} catch (final Exception ex) {
					LOGGER.error("; WARNING: Exception condition -> ", ex);
//				} catch (StackOverflowError err) {
//					LOGGER.error(">> Stack Overflow Error, restarting REP function.", err);
//				} catch (OutOfMemoryError err) {
//					LOGGER.error(">> Out of Memory Error, restarting REP function.", err);
//				} catch (VerifyError ve) {
//					LOGGER.error("; ERROR: loading class, {}", ve.getMessage(), ve);
//				} finally {
//					Variable.Dash.unbind();
				}
			}
		} finally {
			// unbind all of the interaction variables to their original values
//			Variable.Star.unbind();
//			Variable.StarStar.unbind();
//			Variable.StarStarStar.unbind();
//
//			Variable.Plus.unbind();
//			Variable.PlusPlus.unbind();
//			Variable.PlusPlusPlus.unbind();
//
//			Variable.Slash.unbind();
//			Variable.SlashSlash.unbind();
//			Variable.SlashSlashSlash.unbind();
		}

		return null;
	}

	private void generatorTest() throws NoSuchMethodException, InstantiationException, IllegalAccessException, java.lang.reflect.InvocationTargetException {
		final NewLambdaCodeGenerator codeGenerator = context.getBean(NewLambdaCodeGenerator.class);
		final ClassWriter cw = codeGenerator.generate();

		byte[] byteArray = cw.toByteArray();

//						FileOutputStream outputStream = new FileOutputStream(new File("/Volumes/Dev/repo/JCL/TestLambdaGenerator.class"));
//						outputStream.write(byteArray);
//						outputStream.close();

		ClassReader cr = new ClassReader(byteArray);
		CheckClassAdapter cca = new CheckClassAdapter(new MethodEmptyVisitor());

//						System.out.println("Printing the class " + "jcl.TestLambdaGenerator" + '\n');
//						CheckClassAdapter.verify(new ClassReader(byteArray), true, new java.io.PrintWriter(System.out));
//						System.out.println("Done  with class " + "jcl.TestLambdaGenerator" + '\n');

		cr.accept(cca, 0); //ClassReader.EXPAND_FRAMES);

		final CompilerClassLoader cl = CompilerClassLoader.Loader;

		Class<?> classLoaded = cl.loadClass(byteArray, "jcl.TestLambdaGenerator");
		Constructor<?> constructor = classLoaded.getDeclaredConstructor();
		constructor.setAccessible(true);
		final FunctionStruct lambda = (FunctionStruct) constructor.newInstance();
		constructor.setAccessible(false);
		LOGGER.info("GENERATED CLASS -> " + printer.print(lambda));
		LOGGER.info("Result -> " + printer.print(lambda.apply()));
	}

	class MethodEmptyVisitor extends EmptyVisitor {

		@Override
		public MethodVisitor visitMethod(int access, String name, String desc,
		                                 String signature, String[] exceptions) {
			MethodVisitor mv = super.visitMethod(access, name, desc, signature, exceptions);
			CheckMethodAdapter cma = new CheckMethodAdapter(mv);
			return cma;
		}
	}
}
