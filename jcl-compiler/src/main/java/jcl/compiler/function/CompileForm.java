/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import java.io.File;
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
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.extern.slf4j.Slf4j;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class CompileForm {

	public static boolean OUTPUT_FILE = true;

	private final SemanticAnalyzer semanticAnalyzer;
	private final IntermediateCodeGenerator intermediateCodeGenerator;

	public CompileForm(final SemanticAnalyzer semanticAnalyzer,
	                   final IntermediateCodeGenerator intermediateCodeGenerator) {
		this.semanticAnalyzer = semanticAnalyzer;
		this.intermediateCodeGenerator = intermediateCodeGenerator;
	}

	public CompileResult compile(final LispStruct form) {

		final ListStruct lambdaForm = wrapFormInLambda(form);
		final LambdaStruct analyzedObj = semanticAnalyzer.analyze(lambdaForm);
		final Deque<JavaClassBuilder> javaClassBuilderDeque = intermediateCodeGenerator.generate(analyzedObj);

		boolean compiledWithWarnings = false;
		boolean failedToCompile = false;

		FunctionStruct function = null;
		for (final JavaClassBuilder javaClassBuilder : javaClassBuilderDeque) {
			final ClassWriter cw = javaClassBuilder.getClassWriter();

			final byte[] byteArray = cw.toByteArray();

			// TODO: Maybe set this up as a super debugging variable that we can control or something???
			if (OUTPUT_FILE) {
				final String fileName = javaClassBuilder.getFileName();
				try (FileOutputStream outputStream = new FileOutputStream(new File("/Volumes/Dev/repo/JCL/tmp/" + fileName + ".class"))) {
					outputStream.write(byteArray);
				} catch (final IOException ioe) {
					log.info("Error writing class file.", ioe);
				}
			}

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
					function = (FunctionStruct) classLoaded.getDeclaredConstructor().newInstance();
					function.afterPropertiesSet();
				}
			} catch (final Exception ex) {
				log.error("Error compiling definition.", ex);
				compiledWithWarnings = true;
				failedToCompile = true;
			}
		}

		return new CompileResult(function, compiledWithWarnings, failedToCompile);
	}

	private static ListStruct wrapFormInLambda(final LispStruct form) {
		final List<LispStruct> lambdaFormList = new ArrayList<>();
		lambdaFormList.add(SpecialOperatorStructImpl.LAMBDA);
		lambdaFormList.add(NILStruct.INSTANCE);
		lambdaFormList.add(form);

		return ListStruct.toLispList(lambdaFormList);
	}
}
