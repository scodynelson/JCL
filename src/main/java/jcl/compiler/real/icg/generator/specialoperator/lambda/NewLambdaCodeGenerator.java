/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.lambda;

import java.security.SecureRandom;
import java.util.List;
import java.util.Random;
import java.util.Stack;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.PrognCodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
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
public class NewLambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {

		final String fileName = "Lambda" + '_' + System.nanoTime();
		final String className = "jcl/" + fileName;

		final ClassDef currentClass = new ClassDef(className, fileName);
		final Stack<ClassDef> classStack = classBuilder.getClassStack();

		classStack.push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, className, null, "jcl/functions/FunctionStruct", null);

		cw.visitSource(fileName + ".java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			final StringStruct docString = input.getDocString();
			String documentation = "";
			if (docString != null) {
				documentation = docString.getAsJavaString();
			}
			mv.visitLdcInsn(documentation);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/functions/FunctionStruct", "<init>", "(Ljava/lang/String;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "initLambdaListBindings", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final OrdinaryLambdaListBindings lambdaListBindings = input.getLambdaListBindings();

			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initLambdaListBindings", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();

			// OLD: Start
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int requiredStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, requiredStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int optionalStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, optionalStore);

			mv.visitInsn(Opcodes.ACONST_NULL);
			final int restStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, restStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int keyStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, keyStore);

			mv.visitInsn(Opcodes.ICONST_0);
			final int allowOtherKeysStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int auxStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, auxStore);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalStore);
			mv.visitVarInsn(Opcodes.ALOAD, restStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxStore);
			mv.visitInsn(Opcodes.ICONST_0);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings", "<init>", "(Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V", false);

			mv.visitFieldInsn(Opcodes.PUTFIELD, className, "lambdaListBindings", "Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;");

			mv.visitInsn(Opcodes.RETURN);
			// OLD: End

			// NEW: Start
//			final int packageStore = currentClass.getNextAvailableStore();
//			final int allocationStore = currentClass.getNextAvailableStore();
//			final int suppliedPAllocationStore = currentClass.getNextAvailableStore();
//
//			int parameterCounter = 0;
//
//			// End: Required
//			mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
//			mv.visitInsn(Opcodes.DUP);
//			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
//			final int requiredBindingsStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, requiredBindingsStore);
//
//			final int requiredSymbolStore = currentClass.getNextAvailableStore();
//			final int requiredBindingStore = currentClass.getNextAvailableStore();
//
//			final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
//			for (final RequiredBinding requiredBinding : requiredBindings) {
//				final SymbolStruct<?> requiredSymbol = requiredBinding.getSymbolStruct();
//
//				final String packageName = requiredSymbol.getSymbolPackage().getName();
//				final String symbolName = requiredSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, requiredSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitLdcInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, allocationStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/RequiredBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, requiredSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/RequiredBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, 5);
//
//				mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
//				mv.visitVarInsn(Opcodes.ALOAD, requiredBindingStore);
//				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
//				mv.visitInsn(Opcodes.POP);
//			}
//			// End: Required
//
//			// Start: Optional
//			mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
//			mv.visitInsn(Opcodes.DUP);
//			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
//			final int optionalBindingsStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, optionalBindingsStore);
//
//			final int optionalSymbolStore = currentClass.getNextAvailableStore();
//			final int optionalInitFormStore = currentClass.getNextAvailableStore();
//			final int optionalSuppliedPSymbolStore = currentClass.getNextAvailableStore();
//			final int optionalSuppliedPStore = currentClass.getNextAvailableStore();
//			final int optionalBindingStore = currentClass.getNextAvailableStore();
//
//			final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
//			for (final OptionalBinding optionalBinding : optionalBindings) {
//				final SymbolStruct<?> optionalSymbol = optionalBinding.getSymbolStruct();
//
//				String packageName = optionalSymbol.getSymbolPackage().getName();
//				String symbolName = optionalSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, optionalSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, allocationStore);
//
//				final LispStruct optionalInitForm = optionalBinding.getInitForm();
//				formGenerator.generate(optionalInitForm, classBuilder);
//				mv.visitVarInsn(Opcodes.ASTORE, optionalInitFormStore);
//
//				// Start: Supplied-P
//				final SuppliedPBinding suppliedPBinding = optionalBinding.getSuppliedPBinding();
//				final SymbolStruct<?> optionalSuppliedPSymbol = suppliedPBinding.getSymbolStruct();
//
//				packageName = optionalSuppliedPSymbol.getSymbolPackage().getName();
//				symbolName = optionalSuppliedPSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitLdcInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, suppliedPAllocationStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, suppliedPAllocationStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, optionalSuppliedPStore);
//				// End: Supplied-P
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OptionalBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, optionalSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
//				mv.visitVarInsn(Opcodes.ALOAD, optionalInitFormStore);
//				mv.visitVarInsn(Opcodes.ALOAD, optionalSuppliedPStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OptionalBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;Ljcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, optionalBindingStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
//				mv.visitVarInsn(Opcodes.ALOAD, optionalBindingStore);
//				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
//				mv.visitInsn(Opcodes.POP);
//			}
//			// End: Optional
//
//			// Start: Rest
//			final int restSymbolStore = currentClass.getNextAvailableStore();
//			final int restBindingStore = currentClass.getNextAvailableStore();
//
//			final RestBinding restBinding = lambdaListBindings.getRestBinding();
//			if (restBinding == null) {
//				mv.visitInsn(Opcodes.ACONST_NULL);
//				mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
//			} else {
//				final SymbolStruct<?> restSymbol = restBinding.getSymbolStruct();
//
//				final String packageName = restSymbol.getSymbolPackage().getName();
//				final String symbolName = restSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, restSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitLdcInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, allocationStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/RestBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, restSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/RestBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, restBindingStore);
//			}
//			// End: Rest
//
//			// Start: Key
//			mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
//			mv.visitInsn(Opcodes.DUP);
//			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
//			final int keyBindingsStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, keyBindingsStore);
//
//			final int keySymbolStore = currentClass.getNextAvailableStore();
//			final int keyInitFormStore = currentClass.getNextAvailableStore();
//			final int keyNameStore = currentClass.getNextAvailableStore();
//			final int keySuppliedPSymbolStore = currentClass.getNextAvailableStore();
//			final int keySuppliedPStore = currentClass.getNextAvailableStore();
//			final int keyBindingStore = currentClass.getNextAvailableStore();
//
//			final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
//			for (final KeyBinding keyBinding : keyBindings) {
//				final SymbolStruct<?> keySymbol = keyBinding.getSymbolStruct();
//
//				String packageName = keySymbol.getSymbolPackage().getName();
//				String symbolName = keySymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, keySymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, allocationStore);
//
//				final LispStruct keyInitForm = keyBinding.getInitForm();
//				formGenerator.generate(keyInitForm, classBuilder);
//				mv.visitVarInsn(Opcodes.ASTORE, keyInitFormStore);
//
//				mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/packages/GlobalPackageStruct", "KEYWORD", "Ljcl/packages/PackageStruct;");
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				final KeywordStruct keyName = keyBinding.getKeyName();
//				final String keyNameName = keyName.getName();
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(keyNameName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/KeywordStruct");
//				mv.visitVarInsn(Opcodes.ASTORE, keyNameStore);
//
//				// Start: Supplied-P
//				final SuppliedPBinding suppliedPBinding = keyBinding.getSuppliedPBinding();
//				final SymbolStruct<?> keySuppliedPSymbol = suppliedPBinding.getSymbolStruct();
//
//				packageName = keySuppliedPSymbol.getSymbolPackage().getName();
//				symbolName = keySuppliedPSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitLdcInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, suppliedPAllocationStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, suppliedPAllocationStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, keySuppliedPStore);
//				// End: Supplied-P
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/KeyBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, keySymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
//				mv.visitVarInsn(Opcodes.ALOAD, keyInitFormStore);
//				mv.visitVarInsn(Opcodes.ALOAD, keyNameStore);
//				mv.visitVarInsn(Opcodes.ALOAD, keySuppliedPStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/KeyBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;Ljcl/symbols/KeywordStruct;Ljcl/compiler/real/environment/binding/lambdalist/SuppliedPBinding;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, keyBindingStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
//				mv.visitVarInsn(Opcodes.ALOAD, keyBindingStore);
//				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
//				mv.visitInsn(Opcodes.POP);
//			}
//			// End: Key
//
//			// Start: Allow-Other-Keys
//			final int allowOtherKeysStore = currentClass.getNextAvailableStore();
//
//			final boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();
//			if (allowOtherKeys) {
//				mv.visitInsn(Opcodes.ICONST_1);
//				mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
//			} else {
//				mv.visitInsn(Opcodes.ICONST_0);
//				mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);
//			}
//
//			// End: Allow-Other-Keys
//
//			// Start: Aux
//			mv.visitTypeInsn(Opcodes.NEW, "java/util/ArrayList");
//			mv.visitInsn(Opcodes.DUP);
//			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false);
//			final int auxBindingsStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, auxBindingsStore);
//
//			final int auxSymbolStore = currentClass.getNextAvailableStore();
//			final int auxInitFormStore = currentClass.getNextAvailableStore();
//			final int auxBindingStore = currentClass.getNextAvailableStore();
//
//			final List<AuxBinding> auxBindings = lambdaListBindings.getAuxBindings();
//			for (final AuxBinding auxBinding : auxBindings) {
//				final SymbolStruct<?> auxSymbol = auxBinding.getSymbolStruct();
//
//				final String packageName = auxSymbol.getSymbolPackage().getName();
//				final String symbolName = auxSymbol.getName();
//
//				mv.visitLdcInsn(packageName);
//				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, packageStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, packageStore);
//				mv.visitLdcInsn(symbolName);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
//				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
//				mv.visitVarInsn(Opcodes.ASTORE, auxSymbolStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/allocation/ParameterAllocation");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitInsn(parameterCounter++);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/allocation/ParameterAllocation", "<init>", "(I)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, allocationStore);
//
//				final LispStruct auxInitForm = auxBinding.getInitForm();
//				formGenerator.generate(auxInitForm, classBuilder);
//				mv.visitVarInsn(Opcodes.ASTORE, auxInitFormStore);
//
//				mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/AuxBinding");
//				mv.visitInsn(Opcodes.DUP);
//				mv.visitVarInsn(Opcodes.ALOAD, auxSymbolStore);
//				mv.visitVarInsn(Opcodes.ALOAD, allocationStore);
//				mv.visitVarInsn(Opcodes.ALOAD, auxInitFormStore);
//				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/AuxBinding", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/compiler/real/environment/allocation/ParameterAllocation;Ljcl/LispStruct;)V", false);
//				mv.visitVarInsn(Opcodes.ASTORE, auxBindingStore);
//
//				mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
//				mv.visitVarInsn(Opcodes.ALOAD, auxBindingStore);
//				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z", true);
//				mv.visitInsn(Opcodes.POP);
//			}
//			// Start: End
//
//			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
//
//			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings");
//			mv.visitInsn(Opcodes.DUP);
//			mv.visitVarInsn(Opcodes.ALOAD, requiredBindingsStore);
//			mv.visitVarInsn(Opcodes.ALOAD, optionalBindingsStore);
//			mv.visitVarInsn(Opcodes.ALOAD, restBindingStore);
//			mv.visitVarInsn(Opcodes.ALOAD, keyBindingsStore);
//			mv.visitVarInsn(Opcodes.ALOAD, auxBindingsStore);
//			mv.visitVarInsn(Opcodes.ALOAD, allowOtherKeysStore);
//			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings", "<init>", "(Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V", false);
//
//			mv.visitFieldInsn(Opcodes.PUTFIELD, "jcl/compiler/real/icg/generator/testground/TestLambdaGenerator", "lambdaListBindings", "Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;");
//
//			mv.visitInsn(Opcodes.RETURN);
			// NEW: END

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		{
			final PrognStruct forms = input.getForms();
			final LambdaEnvironment lambdaEnvironment = input.getLambdaEnvironment();

			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS, "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();
			final int thisStore = currentClass.getNextAvailableStore();
			final int argsStore = currentClass.getNextAvailableStore();

			// OLD: Start
			prognCodeGenerator.generate(forms, classBuilder);

			mv.visitInsn(Opcodes.ARETURN);
			// OLD: End

			// NEW: Start
//			final Label tryBlockStart = new Label();
//			final Label tryBlockEnd = new Label();
//			final Label catchBlockStart = new Label();
//			final Label catchBlockEnd = new Label();
//			mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
//
//			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
//			mv.visitVarInsn(Opcodes.ALOAD, argsStore);
//			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "getFunctionBindings", "([Ljcl/LispStruct;)Ljava/util/Map;", false);
//			final int functionBindingsStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, functionBindingsStore);
//
//			mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;", true);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
//			final int iteratorStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, iteratorStore);
//
//			final Label iteratorLoopStart = new Label();
//			final Label iteratorLoopEnd = new Label();
//
//			mv.visitLabel(iteratorLoopStart);
//			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);
//
//			mv.visitJumpInsn(Opcodes.IFEQ, iteratorLoopEnd);
//			mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
//			mv.visitTypeInsn(Opcodes.CHECKCAST, "java/util/Map$Entry");
//			final int bindingMapEntryStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, bindingMapEntryStore);
//
//			mv.visitVarInsn(Opcodes.ALOAD, bindingMapEntryStore);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;", true);
//			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
//			final int symbolToBindStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, symbolToBindStore);
//
//			mv.visitVarInsn(Opcodes.ALOAD, bindingMapEntryStore);
//			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;", true);
//			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/LispStruct");
//			final int lexicalValueStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, lexicalValueStore);
//
//			mv.visitVarInsn(Opcodes.ALOAD, symbolToBindStore);
//			mv.visitVarInsn(Opcodes.ALOAD, lexicalValueStore);
//			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);
//
//			mv.visitJumpInsn(Opcodes.GOTO, iteratorLoopStart);
//
//			mv.visitLabel(iteratorLoopEnd);
//			mv.visitLabel(tryBlockStart);
//
//			final Stack<Environment> bindingStack = classBuilder.getBindingStack();
//
//			bindingStack.push(lambdaEnvironment);
//			prognCodeGenerator.generate(forms, classBuilder);
//			bindingStack.pop();
//
//			final int resultStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, resultStore);
//
//			mv.visitLabel(tryBlockEnd);
//			generateFinallyCode(currentClass, mv, functionBindingsStore);
//			mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);
//
//			mv.visitLabel(catchBlockStart);
//			final int exceptionStore = currentClass.getNextAvailableStore();
//			mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);
//
//			generateFinallyCode(currentClass, mv, functionBindingsStore);
//
//			mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
//			mv.visitInsn(Opcodes.ATHROW);
//
//			mv.visitLabel(catchBlockEnd);
//			mv.visitVarInsn(Opcodes.ALOAD, resultStore);
//
//			mv.visitInsn(Opcodes.ARETURN);
			// NEW: End

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			currentClass.resetStores();
		}
		cw.visitEnd();

		classStack.pop();
		if (!classStack.isEmpty()) {
			final ClassDef previousClassDef = classStack.peek();
			classBuilder.setCurrentClass(previousClassDef);
			final MethodVisitor mv = previousClassDef.getMethodVisitor();

			mv.visitTypeInsn(Opcodes.NEW, className);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "()V", false);
		}
	}

	private void generateFinallyCode(final ClassDef currentClass, final MethodVisitor mv, final int functionBindingsStore) {
		mv.visitVarInsn(Opcodes.ALOAD, functionBindingsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "keySet", "()Ljava/util/Set;", true);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;", true);
		final int iteratorStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, iteratorStore);

		final Label iteratorLoopStart = new Label();
		final Label iteratorLoopEnd = new Label();

		mv.visitLabel(iteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z", true);

		mv.visitJumpInsn(Opcodes.IFEQ, iteratorLoopEnd);
		mv.visitVarInsn(Opcodes.ALOAD, iteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;", true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/SymbolStruct");
		final int symbolToUnbindStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, symbolToUnbindStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolToUnbindStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
		mv.visitJumpInsn(Opcodes.GOTO, iteratorLoopStart);

		mv.visitLabel(iteratorLoopEnd);
	}
}
