/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.functions;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Deque;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.functions.FunctionStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.system.CompilerClassLoader;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class CompileForm implements Serializable {

	private static final long serialVersionUID = -8967661954514725036L;

	private static final Logger LOGGER = LoggerFactory.getLogger(CompileForm.class);

	@Autowired
	private SemanticAnalyzer semanticAnalyzer;

	@Autowired
	private IntermediateCodeGenerator intermediateCodeGenerator;

	public CompileResult compile(final LispStruct form) {

		final LambdaStruct analyzedObj = semanticAnalyzer.analyze(form);
		final Deque<ClassDef> classDefDeque = intermediateCodeGenerator.generate(analyzedObj);

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		BooleanStruct failedToCompile = NILStruct.INSTANCE;

		FunctionStruct function = null;
		for (final ClassDef classDef : classDefDeque) {
			final ClassWriter cw = classDef.getClassWriter();

			final byte[] byteArray = cw.toByteArray();

			final ClassReader cr = new ClassReader(byteArray);

			String className = classDef.getName();
			className = className.replace('/', '.');

			final CheckClassAdapter cca = new CheckClassAdapter(new ClassWriter(0), false);
			cr.accept(cca, ClassReader.SKIP_DEBUG + ClassReader.SKIP_FRAMES);

			final CompilerClassLoader cl = CompilerClassLoader.Loader;

			final Class<?> classLoaded = cl.loadClass(byteArray, className);

			Constructor<?> constructor = null;
			try {
				constructor = classLoaded.getConstructor();
				constructor.setAccessible(true);

				function = (FunctionStruct) constructor.newInstance();
			} catch (InstantiationException | NoSuchMethodException | InvocationTargetException | IllegalAccessException ex) {
				LOGGER.error("Error compiling definition.", ex);
				compiledWithWarnings = TStruct.INSTANCE;
				failedToCompile = TStruct.INSTANCE;
			} finally {
				if (constructor != null) {
					constructor.setAccessible(false);
				}
			}
		}

		return new CompileResult(function, compiledWithWarnings, failedToCompile);
	}
}
