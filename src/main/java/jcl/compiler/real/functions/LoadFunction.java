/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.old.LispClassLoader;
import jcl.compiler.old.functions.OpenFunction;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageVariables;
import jcl.pathnames.PathnameStruct;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.JCL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class LoadFunction {

	private static final Logger LOGGER = LoggerFactory.getLogger(LoadFunction.class);

	@Autowired
	private EvalFunction evalFunction;

	private LoadFunction() {
	}

	public Object funcall(final Object fileSpec, final Object loadVerbose, final Object loadPrint, final Object ifNotExists,
	                             final Object extFormat) {
		try {
			ReaderVariables.READTABLE.bindLexicalValue(ReaderVariables.READTABLE.getValue());
			PackageVariables.PACKAGE.bindLexicalValue(PackageVariables.PACKAGE.getValue());
			if (fileSpec instanceof SymbolStruct) {
				return loadStartupCode((SymbolStruct) fileSpec, loadVerbose, loadPrint, ifNotExists, extFormat);
			} else {
				File loadFile = null;
				if (fileSpec instanceof PathnameStruct) {
//                    loadFile = new File(Namestring.FUNCTION.funcall(fileSpec).toString()); TODO
				} else if (fileSpec instanceof String) {
					loadFile = new File(fileSpec.toString());
				} else if (fileSpec instanceof String) {
					loadFile = new File((String) fileSpec);
				} else { // Wrong Type
					throw new RuntimeException("LOAD: Expected a Pathname or a String, got " + fileSpec.getClass());
				}

				if (isCompiledCode(loadFile)) {
					return loadCompiledCode(loadFile, loadVerbose, loadPrint, ifNotExists, extFormat);
				} else {
					return loadSourceCode(loadFile, loadVerbose, loadPrint, ifNotExists, extFormat);
				}
			}
		} finally {
			((SymbolStruct) ReaderVariables.READTABLE).unbindLexicalValue();
			((SymbolStruct) PackageVariables.PACKAGE).unbindLexicalValue();
		}
	}

	private static boolean isCompiledCode(final File fileSpec) {
		return fileSpec.toString().endsWith(".lar") || fileSpec.toString().endsWith(".jar");
	}

	private Object loadSourceCode(final File loadFile, final Object loadVerbose, final Object loadPrint,
	                                     final Object ifNotExists, final Object extFormat) {
		try {
			final ListStruct holderList = ListStruct.buildProperList(new StringStruct(loadFile.toString()));
			final Object file = OpenFunction.FUNCTION.funcall(holderList);

			final Object eofValue = new Object();

			if (loadVerbose != NILStruct.INSTANCE) {
				LOGGER.info("; Loading file {}", loadFile);
			}

			LispStruct readValue;
			while ((readValue =
					null //Read.FUNCTION.funcall(file, NILStruct.INSTANCE, eofValue) TODO
			) != eofValue) {
				try {
					final Object item = evalFunction.apply(readValue);
					if (loadPrint != NullStruct.INSTANCE) {
						LOGGER.info(item.toString());
					}
				} catch (RuntimeException e) {
					LOGGER.warn("Exception in reading: {}", readValue, e);
				}
			}
			return TStruct.INSTANCE;
		} catch (final RuntimeException e) {
			LOGGER.warn("RuntimeException", e);
			if (ifNotExists == NILStruct.INSTANCE) {
				return NILStruct.INSTANCE;
			} else {
				throw new RuntimeException("file " + loadFile + " does not exist");
			}
		}
	}

	private static Object loadCompiledCode(final File loadFile, final Object loadVerbose, final Object loadPrint,
	                                       final Object ifNotExists, final Object extFormat) {
		try {
			LOGGER.info("loadFile: {}", loadFile);
			final JarFile jf = new JarFile(loadFile);
			final LispClassLoader loader = new LispClassLoader(jf);

			final Manifest manifest = jf.getManifest();
			final Attributes att = manifest.getMainAttributes();

			final String main = att.get(Attributes.Name.MAIN_CLASS).toString();

			final Class<?> lambda2 = Class.forName(main, true, loader);
			final Constructor<?> construct = lambda2.getConstructor();

			return ((FunctionStruct) construct.newInstance()).apply();

		} catch (final ClassNotFoundException e) {
			LOGGER.error("ClassNotFoundException", e);
		} catch (final InstantiationException e) {
			LOGGER.error("InstantiationException", e);
		} catch (final IllegalAccessException e) {
			LOGGER.error("IllegalAccessException", e);
		} catch (final InvocationTargetException e) {
			LOGGER.error("InvocationTargetException", e);
		} catch (final NoSuchMethodException e) {
			LOGGER.error("NoSuchMethodException", e);
		} catch (final IOException e) {
			LOGGER.error("IOException", e);
		} catch (final IllegalArgumentException e) {
			LOGGER.error("IllegalArgumentException", e);
		} catch (final SecurityException e) {
			LOGGER.error("SecurityException", e);
		} catch (final RuntimeException e) {
			LOGGER.error("Exception", e);
		}
		return null;
	}

	private static Object loadStartupCode(final SymbolStruct<?> loadFile, final Object loadVerbose, final Object loadPrint,
	                                      final Object ifNotExists, final Object extFormat) {
		try {
			// turn the Symbol into a string
			final String main = loadFile.toString();

			// get the current class loader
			final ClassLoader loader = JCL.CURRENT_CLASSLOADER;
			// load the main lambda in the jar file
			loader.loadClass(main);
			final Class<?> lambda2 = Class.forName(main, true, loader);
			final Constructor<?> construct = lambda2.getConstructor();
			// run the wrap-around lambda expression
			return ((FunctionStruct) construct.newInstance()).apply();

		} catch (final ClassNotFoundException e) {
			LOGGER.error("WARNING: class {} not found during loading of initialization JAR file.", loadFile, e);
		} catch (final InstantiationException e) {
			LOGGER.error("InstantiationException", e);
		} catch (final IllegalAccessException e) {
			LOGGER.error("IllegalAccessException", e);
		} catch (final InvocationTargetException e) {
			LOGGER.error("InvocationTargetException", e);
		} catch (final NoSuchMethodException e) {
			LOGGER.error("NoSuchMethodException", e);
		} catch (final IllegalArgumentException e) {
			LOGGER.error("IllegalArgumentException", e);
		} catch (final SecurityException e) {
			LOGGER.error("SecurityException", e);
		} catch (final RuntimeException e) {
			LOGGER.error("Exception", e);
		}
		return null;
	}
}
