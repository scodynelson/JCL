/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

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
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.TStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.FunctionStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class CompileForm {

	private static final Logger LOGGER = LoggerFactory.getLogger(CompileForm.class);

	public static boolean OUTPUT_FILE = true;

	@Autowired
	private SemanticAnalyzer semanticAnalyzer;

	@Autowired
	private IntermediateCodeGenerator intermediateCodeGenerator;

	@Autowired
	private ConfigurableApplicationContext applicationContext;

	public CompileResult compile(final LispStruct form) {

		final ListStruct lambdaForm = wrapFormInLambda(form);
		final LambdaStruct analyzedObj = semanticAnalyzer.analyze(lambdaForm);
		final Deque<JavaClassBuilder> javaClassBuilderDeque = intermediateCodeGenerator.generate(analyzedObj);

		BooleanStruct compiledWithWarnings = NILStruct.INSTANCE;
		BooleanStruct failedToCompile = NILStruct.INSTANCE;

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
					LOGGER.info("Error writing class file.", ioe);
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
					final BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(classLoaded);
					final DefaultListableBeanFactory factory = (DefaultListableBeanFactory) applicationContext.getBeanFactory();

					final String beanName = classLoaded.getSimpleName();
					final BeanDefinition beanDefinition = builder.getBeanDefinition();
					factory.registerBeanDefinition(beanName, beanDefinition);

					function = (FunctionStruct) applicationContext.getBean(classLoaded);
				}
			} catch (BeansException | IllegalStateException ex) {
				LOGGER.error("Error compiling definition.", ex);
				compiledWithWarnings = TStruct.INSTANCE;
				failedToCompile = TStruct.INSTANCE;
			}
		}

		return new CompileResult(function, compiledWithWarnings, failedToCompile);
	}

	private static ListStruct wrapFormInLambda(final LispStruct form) {
		final List<LispStruct> lambdaFormList = new ArrayList<>();
		lambdaFormList.add(SpecialOperatorStruct.LAMBDA);
		lambdaFormList.add(NILStruct.INSTANCE);
		lambdaFormList.add(form);

		return LispStructFactory.toProperList(lambdaFormList);
	}
}
