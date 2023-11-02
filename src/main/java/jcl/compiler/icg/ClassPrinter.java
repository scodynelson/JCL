package jcl.compiler.icg;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;

@Log4j2
@UtilityClass
public final class ClassPrinter {

	public static boolean OUTPUT_FILE;

	public static void outputCompiledClassFile(final JavaClassBuilder javaClassBuilder, final byte[] byteArray) {
		// TODO: Maybe set this up as a super debugging variable that we can control or something???
		if (OUTPUT_FILE) {
			final String fileName = javaClassBuilder.getFileName();
			final String tmpDir = "/Users/codynelson/workspace/JCL/compiled-lisp/tmp/";
			final File tmpDirFile = new File(tmpDir);
			if (!tmpDirFile.exists()) {
				tmpDirFile.mkdir();
			}
			try (final FileOutputStream outputStream = new FileOutputStream(tmpDir + fileName + ".class")) {
				outputStream.write(byteArray);
			} catch (final IOException ioe) {
				log.info("Error writing class file.", ioe);
			}
		}
	}
}
