/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.security.SecureRandom;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.simple.NullCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.PrognCodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.NullStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {

		final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();
		final StringStruct docString = input.getDocString();
		final PrognStruct forms = input.getForms();
		final LambdaEnvironment lambdaEnvironment = input.getLambdaEnvironment();

		String fileName = input.getFileName();
		fileName = fileName.replace('.', '/');

		final String className = fileName.substring(fileName.lastIndexOf('/') + 1, fileName.length());

		final ClassDef currentClass = new ClassDef(fileName, className);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, fileName, null, "jcl/functions/FunctionStruct", null);

		cw.visitSource(className + ".java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final List<LoadTimeValue> loadTimeValues = lambdaEnvironment.getLoadTimeValues();
			for (final LoadTimeValue loadTimeValue : loadTimeValues) {
				final String uniqueLTVId = loadTimeValue.getUniqueLTVId();

				final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, uniqueLTVId, "Ljcl/LispStruct;", null, null);
				fv.visitEnd();
			}
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, fileName, "<init>", "(Ljcl/functions/Closure;)V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "(Ljcl/functions/Closure;)V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int closureStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			String documentation = "";
			if (docString != null) {
				documentation = docString.getAsJavaString();
			}
			mv.visitLdcInsn(documentation);
			mv.visitVarInsn(Opcodes.ALOAD, closureStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/functions/FunctionStruct", "<init>", "(Ljava/lang/String;Ljcl/functions/Closure;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, fileName, "initLambdaListBindings", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initLambdaListBindings", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			final int packageStore = currentClass.getNextAvailableStore();
			final int allocationStore = currentClass.getNextAvailableStore();
			final int suppliedPAllocationStore = currentClass.getNextAvailableStore();

			int parameterCounter = 0;

			// End: Required
			final int requiredBindingsStore = currentClass.getNextAvailableStore();
			parameterCounter = generateRequiredBindings(currentClass, lambdaListBindings, mv, packageStore, allocationStore, parameterCounter, requiredBindingsStore);
			// End: Required

			// Start: Optional
			final int optionalBindingsStore = currentClass.getNextAvailableStore();
			parameterCounter = generateOptionalBindings(classBuilder, currentClass, lambdaListBindings, mv, packageStore, allocationStore, suppliedPAllocationStore, parameterCounter, optionalBindingsStore);
			// End: Optional

			// Start: Rest
			final int restBindingStore = currentClass.getNextAvailableStore();
			parameterCounter = generateRestBinding(currentClass, lambdaListBindings, mv, packageStore, allocationStore, parameterCounter, restBindingStore);
			// End: Rest

			// Start: Key
			final int keyBindingsStore = currentClass.getNextAvailableStore();
			parameterCounter = generateKeyBindings(classBuilder, currentClass, lambdaListBindings, mv, packageStore, allocationStore, suppliedPAllocationStore, parameterCounter, keyBindingsStore);
			// End: Key

			// Start: Allow-Other-Keys
			final int allowOtherKeysStore = currentClass.getNextAvailableStore();
			generateAllowOtherKeys(lambdaListBindings, mv, allowOtherKeysStore);
			// End: Allow-Other-Keys

			// Start: Aux
			final int auxBindingsStore = currentClass.getNextAvailableStore();
			generateAuxBindings(classBuilder, currentClass, lambdaListBindings, mv, packageStore, allocationStore, parameterCounter, auxBindingsStore);
			// Start: End

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, restBindingStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
			mv.visitVarInsn(Opcodes.ILOAD, allowOtherKeysStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings", "<init>", "(Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V", false);

			mv.visitFieldInsn(Opcodes.PUTFIELD, fileName, "lambdaListBindings", "Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;");

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS, "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int argsStore = currentClass.getNextAvailableStore();

			final Label tryBlockStart = new Label();
			final Label tryBlockEnd = new Label();
			final Label catchBlockStart = new Label();
			final Label catchErrorExceptionStart = new Label();
			final Label catchThrowableStart = new Label();
			final Label finallyBlockStart = new Label();
			final Label finallyBlockEnd = new Label();
			mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchErrorExceptionStart, "jcl/conditions/exceptions/ErrorException");
			mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchThrowableStart, "java/lang/Throwable");
			mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
			mv.visitTryCatchBlock(catchErrorExceptionStart, finallyBlockStart, catchBlockStart, null);

			// START: Bind Closure Symbol values
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, "getClosureSymbolBindings", "()Ljava/util/Map;", false);
			final int closureSymbolBindingsStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int closureSymbolBindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingIteratorStore);

			final Label closureSymbolBindingIteratorLoopStart = new Label();
			final Label closureSymbolBindingIteratorLoopEnd = new Label();

			mv.visitLabel(closureSymbolBindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, closureSymbolBindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "java/util/Map$Entry");
			final int closureSymbolBindingMapEntryStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingMapEntryStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int closureSymbolToBindStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolToBindStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
			final int closureSymbolLexicalValueStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolLexicalValueStore);

			final Label closureSymbolValuesCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
			mv.visitJumpInsn(Opcodes.IFEQ, closureSymbolValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
			final int closureSymbolValuesStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolValuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolValuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, closureSymbolLexicalValueStore);

			mv.visitLabel(closureSymbolValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolToBindStore);
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolLexicalValueStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);
			mv.visitJumpInsn(Opcodes.GOTO, closureSymbolBindingIteratorLoopStart);

			mv.visitLabel(closureSymbolBindingIteratorLoopEnd);
			// END: Bind Closure Symbol values

			// START: Bind Closure Function values
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, "getClosureFunctionBindings", "()Ljava/util/Map;", false);
			final int closureFunctionBindingsStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int closureFunctionBindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingIteratorStore);

			final Label closureFunctionBindingIteratorLoopStart = new Label();
			final Label closureFunctionBindingIteratorLoopEnd = new Label();

			mv.visitLabel(closureFunctionBindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, closureFunctionBindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "java/util/Map$Entry");
			final int closureFunctionBindingMapEntryStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingMapEntryStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int closureFunctionToBindStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureFunctionToBindStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/functions/FunctionStruct");
			final int closureFunctionValueStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, closureFunctionValueStore);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionToBindStore);
			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionValueStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindFunction", "(Ljcl/functions/FunctionStruct;)V", false);
			mv.visitJumpInsn(Opcodes.GOTO, closureFunctionBindingIteratorLoopStart);

			mv.visitLabel(closureFunctionBindingIteratorLoopEnd);
			// END: Bind Closure Function values

			// START: Bind InitForm values
			bindingStack.push(lambdaEnvironment);

			final int initFormVarPackageStore = currentClass.getNextAvailableStore();
			final int initFormLexicalValueStore = currentClass.getNextAvailableStore();

			final Set<Integer> initFormVarSymbolsToUnbind = new HashSet<>();

			final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
			for (final OptionalBinding optionalBinding : optionalBindings) {
				final SymbolStruct<?> var = optionalBinding.getSymbolStruct();
				final LispStruct initForm = optionalBinding.getInitForm();

				final String packageName = var.getSymbolPackage().getName();
				final String symbolName = var.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, initFormVarPackageStore);

				mv.visitVarInsn(Opcodes.ALOAD, initFormVarPackageStore);
				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
				final int initFormVarSymbolStore = currentClass.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, initFormVarSymbolStore);

				formGenerator.generate(initForm, classBuilder);
				mv.visitVarInsn(Opcodes.ASTORE, initFormLexicalValueStore);

				mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, initFormLexicalValueStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);

				initFormVarSymbolsToUnbind.add(initFormVarSymbolStore);
			}

			final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
			for (final KeyBinding keyBinding : keyBindings) {
				final SymbolStruct<?> var = keyBinding.getSymbolStruct();
				final LispStruct initForm = keyBinding.getInitForm();

				final String packageName = var.getSymbolPackage().getName();
				final String symbolName = var.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, initFormVarPackageStore);

				mv.visitVarInsn(Opcodes.ALOAD, initFormVarPackageStore);
				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
				final int initFormVarSymbolStore = currentClass.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, initFormVarSymbolStore);

				formGenerator.generate(initForm, classBuilder);
				mv.visitVarInsn(Opcodes.ASTORE, initFormLexicalValueStore);

				mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, initFormLexicalValueStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);

				initFormVarSymbolsToUnbind.add(initFormVarSymbolStore);
			}

			bindingStack.pop();
			// END: Bind InitForm values

			// START: Bind Parameter values
			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitVarInsn(Opcodes.ALOAD, argsStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, "getFunctionBindings", "([Ljcl/LispStruct;)Ljava/util/Map;", false);
			final int functionBindingsStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, functionBindingsStore);

			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int parameterBindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterBindingIteratorStore);

			final Label parameterBindingIteratorLoopStart = new Label();
			final Label parameterBindingIteratorLoopEnd = new Label();

			mv.visitLabel(parameterBindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, parameterBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, parameterBindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, parameterBindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "java/util/Map$Entry");
			final int parameterBindingMapEntryStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterBindingMapEntryStore);

			mv.visitVarInsn(Opcodes.ALOAD, parameterBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int parameterSymbolToBindStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterSymbolToBindStore);

			mv.visitVarInsn(Opcodes.ALOAD, parameterBindingMapEntryStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
			final int parameterLexicalValueStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterLexicalValueStore);

			final Label parameterValuesCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, parameterLexicalValueStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
			mv.visitJumpInsn(Opcodes.IFEQ, parameterValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, parameterLexicalValueStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
			final int parameterValuesStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parameterValuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, parameterValuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, parameterLexicalValueStore);

			mv.visitLabel(parameterValuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, parameterSymbolToBindStore);
			mv.visitVarInsn(Opcodes.ALOAD, parameterLexicalValueStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);
			mv.visitJumpInsn(Opcodes.GOTO, parameterBindingIteratorLoopStart);

			mv.visitLabel(parameterBindingIteratorLoopEnd);
			// END: Bind Parameter values

			mv.visitLabel(tryBlockStart);

			bindingStack.push(lambdaEnvironment);
			prognCodeGenerator.generate(forms, classBuilder);
			bindingStack.pop();

			final int resultStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, resultStore);

			mv.visitLabel(tryBlockEnd);

			// START: Unbind Parameter values
			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int normalParameterUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalParameterUnbindingIteratorStore);

			final Label normalParameterUnbindingIteratorLoopStart = new Label();
			final Label normalParameterUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalParameterUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalParameterUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int normalParameterUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalParameterUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalParameterUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, normalParameterUnbindingIteratorLoopStart);

			mv.visitLabel(normalParameterUnbindingIteratorLoopEnd);
			// END: Unbind Parameter values

			// START: Unbind InitForm values
			for (final Integer initFormVarSymbol : initFormVarSymbolsToUnbind) {
				mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbol);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			}
			// END: Unbind InitForm values

			// START: Unbind Closure Function values
			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int normalClosureFunctionUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureFunctionUnbindingIteratorStore);

			final Label normalClosureFunctionUnbindingIteratorLoopStart = new Label();
			final Label normalClosureFunctionUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalClosureFunctionUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalClosureFunctionUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int normalClosureFunctionUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureFunctionUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalClosureFunctionUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindFunction", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, normalClosureFunctionUnbindingIteratorLoopStart);

			mv.visitLabel(normalClosureFunctionUnbindingIteratorLoopEnd);
			// END: Unbind Closure Function values

			// START: Unbind Closure Symbol values
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int normalClosureUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureUnbindingIteratorStore);

			final Label normalClosureSymbolUnbindingIteratorLoopStart = new Label();
			final Label normalClosureSymbolUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(normalClosureSymbolUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, normalClosureSymbolUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int normalClosureUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, normalClosureUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, normalClosureUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, normalClosureSymbolUnbindingIteratorLoopStart);

			mv.visitLabel(normalClosureSymbolUnbindingIteratorLoopEnd);
			// END: Unbind Closure Symbol values

			mv.visitJumpInsn(Opcodes.GOTO, finallyBlockEnd);

			final int exceptionStore = currentClass.getNextAvailableStore();

			mv.visitLabel(catchErrorExceptionStart);
			mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(catchThrowableStart);
			mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/conditions/exceptions/ErrorException");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn("Non-Lisp error found.");
			mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/conditions/exceptions/ErrorException", "<init>", "(Ljava/lang/String;Ljava/lang/Throwable;)V", false);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(catchBlockStart);

			final int finallyExceptionStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, finallyExceptionStore);

			mv.visitLabel(finallyBlockStart);

			// START: Unbind Parameter values
			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int exceptionParameterUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionParameterUnbindingIteratorStore);

			final Label exceptionParameterUnbindingIteratorLoopStart = new Label();
			final Label exceptionParameterUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionParameterUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionParameterUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int exceptionParameterUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionParameterUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionParameterUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionParameterUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionParameterUnbindingIteratorLoopEnd);
			// END: Unbind Parameter values

			// START: Unbind InitForm values
			for (final Integer initFormVarSymbol : initFormVarSymbolsToUnbind) {
				mv.visitVarInsn(Opcodes.ALOAD, initFormVarSymbol);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			}
			// END: Unbind InitForm values

			// START: Unbind Closure Function values
			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int exceptionClosureFunctionUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureFunctionUnbindingIteratorStore);

			final Label exceptionClosureFunctionUnbindingIteratorLoopStart = new Label();
			final Label exceptionClosureFunctionUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionClosureFunctionUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionClosureFunctionUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int exceptionClosureFunctionUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureFunctionUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureFunctionUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindFunction", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionClosureFunctionUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionClosureFunctionUnbindingIteratorLoopEnd);
			// END: Unbind Closure Function values

			// START: Unbind Closure Symbol values
			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
			final int exceptionClosureUnbindingIteratorStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureUnbindingIteratorStore);

			final Label exceptionClosureUnbindingIteratorLoopStart = new Label();
			final Label exceptionClosureUnbindingIteratorLoopEnd = new Label();

			mv.visitLabel(exceptionClosureUnbindingIteratorLoopStart);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

			mv.visitJumpInsn(Opcodes.IFEQ, exceptionClosureUnbindingIteratorLoopEnd);
			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingIteratorStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
			final int exceptionClosureUnbindingMapKeySymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, exceptionClosureUnbindingMapKeySymbolStore);

			mv.visitVarInsn(Opcodes.ALOAD, exceptionClosureUnbindingMapKeySymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
			mv.visitJumpInsn(Opcodes.GOTO, exceptionClosureUnbindingIteratorLoopStart);

			mv.visitLabel(exceptionClosureUnbindingIteratorLoopEnd);
			// END: Unbind Closure Symbol values

			mv.visitVarInsn(Opcodes.ALOAD, finallyExceptionStore);
			mv.visitInsn(Opcodes.ATHROW);

			mv.visitLabel(finallyBlockEnd);
			mv.visitVarInsn(Opcodes.ALOAD, resultStore);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			final int initFormStore = currentClass.getNextAvailableStore();

			final List<LoadTimeValue> loadTimeValues = lambdaEnvironment.getLoadTimeValues();
			for (final LoadTimeValue loadTimeValue : loadTimeValues) {
				final String uniqueLTVId = loadTimeValue.getUniqueLTVId();
				final LispStruct value = loadTimeValue.getValue();

				formGenerator.generate(value, classBuilder);
				mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

				final Label valuesCheckIfEnd = new Label();

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
				mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
				final int valuesStore = currentClass.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

				mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

				mv.visitLabel(valuesCheckIfEnd);

				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitFieldInsn(Opcodes.PUTSTATIC, fileName, uniqueLTVId, "Ljcl/LispStruct;");
			}

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
			final MethodVisitor mv = previousClassDef.getMethodVisitor();

			mv.visitTypeInsn(Opcodes.NEW, fileName);
			mv.visitInsn(Opcodes.DUP);

			final Stack<Integer> closureStoreStack = previousClassDef.getClosureStoreStack();
			if (closureStoreStack.isEmpty()) {
				mv.visitInsn(Opcodes.ACONST_NULL);
			} else {
				final Integer currentClosureStore = closureStoreStack.peek();
				mv.visitVarInsn(Opcodes.ALOAD, currentClosureStore);
			}

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, fileName, "<init>", "(Ljcl/functions/Closure;)V", false);
		}
	}

	private int generateRequiredBindings(final ClassDef currentClass, final OrdinaryLambdaListBindings lambdaListBindings,
	                                     final MethodVisitor mv, final int packageStore, final int allocationStore,
	                                     final int currentParameterCounter, final int requiredBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
		mv.visitVarInsn(Opcodes.ASTORE, requiredBindingsStore);

		final int requiredSymbolStore = currentClass.getNextAvailableStore();
		final int requiredBindingStore = currentClass.getNextAvailableStore();

		int parameterCounter = currentParameterCounter;

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding requiredBinding : requiredBindings) {
			final SymbolStruct<?> requiredSymbol = requiredBinding.getSymbolStruct();

			final String packageName = requiredSymbol.getSymbolPackage().getName();
			final String symbolName = requiredSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, requiredSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(parameterCounter++);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, allocationStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/RequiredBinding");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/RequiredBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, requiredBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);
		}
		return parameterCounter;
	}

	private int generateOptionalBindings(final JavaClassBuilder classBuilder, final ClassDef currentClass,
	                                     final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                     final int packageStore, final int allocationStore, final int suppliedPAllocationStore,
	                                     final int currentParameterCounter, final int optionalBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
		mv.visitVarInsn(Opcodes.ASTORE, optionalBindingsStore);

		final int optionalSymbolStore = currentClass.getNextAvailableStore();
		final int optionalInitFormStore = currentClass.getNextAvailableStore();
		final int optionalSuppliedPSymbolStore = currentClass.getNextAvailableStore();
		final int optionalSuppliedPStore = currentClass.getNextAvailableStore();
		final int optionalBindingStore = currentClass.getNextAvailableStore();

		int parameterCounter = currentParameterCounter;

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding optionalBinding : optionalBindings) {
			final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();
			final LispStruct optionalInitForm = optionalBinding.getInitForm();

			String packageName = optionalSymbol.getSymbolPackage().getName();
			String symbolName = optionalSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, optionalSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(parameterCounter++);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, allocationStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
			} else {
				final SymbolStruct<?> optionalSuppliedPSymbol = suppliedPBinding.getSymbolStruct();

				packageName = optionalSuppliedPSymbol.getSymbolPackage().getName();
				symbolName = optionalSuppliedPSymbol.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, packageStore);

				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPSymbolStore);

				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
				mv.visitInsn(Opcodes.DUP);
				mv.visitLdcInsn(parameterCounter++);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
				mv.visitVarInsn(Opcodes.ASTORE, suppliedPAllocationStore);

				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding");
				mv.visitInsn(Opcodes.DUP);
				mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPSymbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, suppliedPAllocationStore);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
			}
			// End: Supplied-P

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OptionalBinding");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, optionalSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalInitFormStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OptionalBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;Ljcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, optionalBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);
		}
		return parameterCounter;
	}

	private int generateRestBinding(final ClassDef currentClass, final OrdinaryLambdaListBindings lambdaListBindings,
	                                final MethodVisitor mv, final int packageStore, final int allocationStore,
	                                final int currentParameterCounter, final int restBindingStore) {

		final int restSymbolStore = currentClass.getNextAvailableStore();

		int parameterCounter = currentParameterCounter;

		final RestBinding restBinding = lambdaListBindings.getRestBinding();
		if (restBinding == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
		} else {
			final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();

			final String packageName = restSymbol.getSymbolPackage().getName();
			final String symbolName = restSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, restSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(parameterCounter++);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, allocationStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/RestBinding");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, restSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/RestBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
		}
		return parameterCounter;
	}

	private int generateKeyBindings(final JavaClassBuilder classBuilder, final ClassDef currentClass,
	                                final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                final int packageStore, final int allocationStore, final int suppliedPAllocationStore,
	                                final int currentParameterCounter, final int keyBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
		mv.visitVarInsn(Opcodes.ASTORE, keyBindingsStore);

		final int keySymbolStore = currentClass.getNextAvailableStore();
		final int keyInitFormStore = currentClass.getNextAvailableStore();
		final int keyNameStore = currentClass.getNextAvailableStore();
		final int keySuppliedPSymbolStore = currentClass.getNextAvailableStore();
		final int keySuppliedPStore = currentClass.getNextAvailableStore();
		final int keyBindingStore = currentClass.getNextAvailableStore();

		int parameterCounter = currentParameterCounter;

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		for (final KeyBinding keyBinding : keyBindings) {
			final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
			final LispStruct keyInitForm = keyBinding.getInitForm();

			String packageName = keySymbol.getSymbolPackage().getName();
			String symbolName = keySymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, keySymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(parameterCounter++);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, allocationStore);

			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);

			mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/packages/GlobalPackageStruct", "KEYWORD", "Ljcl/packages/PackageStruct;");
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			final KeywordStruct keyName = keyBinding.getKeyName();
			final String keyNameName = keyName.getName();

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(keyNameName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/KeywordStruct");
			mv.visitVarInsn(Opcodes.ASTORE, keyNameStore);

			// Start: Supplied-P
			final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding == null) {
				mv.visitInsn(Opcodes.ACONST_NULL);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
			} else {
				final SymbolStruct<?> keySuppliedPSymbol = suppliedPBinding.getSymbolStruct();

				packageName = keySuppliedPSymbol.getSymbolPackage().getName();
				symbolName = keySuppliedPSymbol.getName();

				mv.visitLdcInsn(packageName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, packageStore);

				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPSymbolStore);

				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
				mv.visitInsn(Opcodes.DUP);
				mv.visitLdcInsn(parameterCounter++);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
				mv.visitVarInsn(Opcodes.ASTORE, suppliedPAllocationStore);

				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding");
				mv.visitInsn(Opcodes.DUP);
				mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPSymbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, suppliedPAllocationStore);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
			}
			// End: Supplied-P

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/KeyBinding");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, keySymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyInitFormStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyNameStore);
			mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/KeyBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;Ljcl/symbols/KeywordStruct;Ljcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, keyBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);
		}
		return parameterCounter;
	}

	private void generateAllowOtherKeys(final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                    final int allowOtherKeysStore) {

		final boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();
		if (allowOtherKeys) {
			mv.visitInsn(Opcodes.ICONST_1);
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
		} else {
			mv.visitInsn(Opcodes.ICONST_0);
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
		}
	}

	private int generateAuxBindings(final JavaClassBuilder classBuilder, final ClassDef currentClass,
	                                final OrdinaryLambdaListBindings lambdaListBindings, final MethodVisitor mv,
	                                final int packageStore, final int allocationStore,
	                                final int currentParameterCounter, final int auxBindingsStore) {

		mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
		mv.visitVarInsn(Opcodes.ASTORE, auxBindingsStore);

		final int auxSymbolStore = currentClass.getNextAvailableStore();
		final int auxInitFormStore = currentClass.getNextAvailableStore();
		final int auxBindingStore = currentClass.getNextAvailableStore();

		int parameterCounter = currentParameterCounter;

		final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
		for (final AuxBinding auxBinding : auxBindings) {
			final SymbolStruct<?> auxSymbol = auxBinding.getSymbolStruct();

			final String packageName = auxSymbol.getSymbolPackage().getName();
			final String symbolName = auxSymbol.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, auxSymbolStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(parameterCounter++);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, allocationStore);

			// NOTE: Just generate a null value for this initForm here. We take care of the &aux initForms in the body
			//       when it is processed
			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, auxInitFormStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/AuxBinding");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, auxSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxInitFormStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/AuxBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, auxBindingStore);

			mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxBindingStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
			mv.visitInsn(Opcodes.POP);
		}
		return parameterCounter;
	}
}