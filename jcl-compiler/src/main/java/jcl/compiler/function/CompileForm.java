/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import jcl.compiler.classloaders.CompilerClassLoader;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.sa.SemanticAnalyzer;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;

@Log4j2
@UtilityClass
public final class CompileForm {

	public static boolean OUTPUT_FILE;

	public static CompileResult compile(final LispStruct form) {

		final ListStruct lambdaForm = wrapFormInLambda(form);
		final LambdaStruct analyzedObj = SemanticAnalyzer.analyze(lambdaForm);
		final Deque<JavaClassBuilder> javaClassBuilderDeque = IntermediateCodeGenerator.generate(analyzedObj);

		boolean compiledWithWarnings = false;
		boolean failedToCompile = false;

		FunctionStruct function = null;
		for (final JavaClassBuilder javaClassBuilder : javaClassBuilderDeque) {
			final ClassWriter cw = javaClassBuilder.getClassWriter();

			final byte[] byteArray = cw.toByteArray();
			outputCompiledClassFile(javaClassBuilder, byteArray);

			final ClassReader cr = new ClassReader(byteArray);

			String className = javaClassBuilder.getClassName();
			className = className.replace('/', '.');

			final CheckClassAdapter cca = new CheckClassAdapter(new ClassWriter(0), false);
			cr.accept(cca, ClassReader.SKIP_DEBUG + ClassReader.SKIP_FRAMES);

			final CompilerClassLoader cl = CompilerClassLoader.INSTANCE;

			final Class<?> classLoaded = cl.loadClass(className, byteArray);

			try {
				final boolean isFunctionStruct = FunctionStruct.class.isAssignableFrom(classLoaded);
				if (isFunctionStruct) {
					function = (FunctionStruct) classLoaded.getConstructor().newInstance();
					final SymbolStruct functionSymbol = function.getFunctionSymbol();
					functionSymbol.setFunction(function);
				}
			} catch (final Exception ex) {
				log.error("Error compiling definition.", ex);
				compiledWithWarnings = true;
				failedToCompile = true;
			}
		}

		return new CompileResult(
				function,
				BooleanStruct.toLispBoolean(compiledWithWarnings),
				BooleanStruct.toLispBoolean(failedToCompile)
		);
	}

	public static void outputCompiledClassFile(final JavaClassBuilder javaClassBuilder, final byte[] byteArray) {
		// TODO: Maybe set this up as a super debugging variable that we can control or something???
		if (OUTPUT_FILE) {
			final String fileName = javaClassBuilder.getFileName();
			final String tmpDir = "/Users/codynelson/workspace/JCL/jcl-application/compiled-lisp/";
			try (final FileOutputStream outputStream = new FileOutputStream(tmpDir + fileName + ".class")) {
				outputStream.write(byteArray);
			} catch (final IOException ioe) {
				log.info("Error writing class file.", ioe);
			}
		}
	}

	private static ListStruct wrapFormInLambda(final LispStruct form) {
		final List<LispStruct> lambdaFormList = new ArrayList<>();
		lambdaFormList.add(CommonLispSymbols.LAMBDA);
		lambdaFormList.add(NILStruct.INSTANCE);
		lambdaFormList.add(form);

		return ListStruct.toLispList(lambdaFormList);
	}
}
