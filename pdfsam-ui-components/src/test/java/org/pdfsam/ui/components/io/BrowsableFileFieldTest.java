/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.components.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.io.FileType;
import org.pdfsam.model.io.OpenType;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class BrowsableFileFieldTest {

    @Test
    public void defaultPromptText() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
        var victim = new BrowsableFileField(FileType.ALL, OpenType.OPEN);
        assertEquals(i18n().tr("Select a file"), victim.getTextField().getPromptText());
    }

    @Test
    public void setTextFromNullFile() {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
        victim.setTextFromFile((File) null);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
    }

    @Test
    public void setTextFromNullPath() {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
        victim.setTextFromFile((Path) null);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
    }

    @Test
    public void validExisting(@TempDir Path folder) throws IOException {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.enforceValidation(true, true);
        var inputFile = Files.createFile(folder.resolve("test.pdf")).toFile();
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidExisting(@TempDir Path folder) throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.enforceValidation(true, true);
        var inputFile = Files.createFile(folder.resolve("test.oss")).toFile();
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidExistingPath(@TempDir Path folder) throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.enforceValidation(true, true);
        var inputFile = Files.createFile(folder.resolve("test.oss"));
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.toAbsolutePath().toString(), victim.getTextField().getText());
    }

    @Test
    public void validNotExisting() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(false, true);
        File inputFile = new File("ChuckNorris/roundhouse/kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void validNotExistingPath(@TempDir Path folder) {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(false, true);
        var inputFile = folder.resolve("kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.toAbsolutePath().toString(), victim.getTextField().getText());
    }

    @Test
    public void invalidNotExisting() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        File inputFile = new File("ChuckNorris/roundhouse/kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidNotExistingPath(@TempDir Path folder) {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        var inputFile = folder.resolve("kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.toAbsolutePath().toString(), victim.getTextField().getText());
    }

    @Test
    public void validSpecialCharsFolderExisting(@TempDir Path folder) throws IOException {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        var inputFile = Files.createFile(folder.resolve("只需要选择需要转换的文件_test.pdf")).toFile();
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void validSpecialCharsFolderExistingPath(@TempDir Path folder) throws IOException {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        var inputFile = Files.createFile(folder.resolve("只需要选择需要转换的文件_test.pdf"));
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.toAbsolutePath().toString(), victim.getTextField().getText());
    }

    @Test
    public void saveState(@TempDir Path folder) {
        var victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.setId("fieldId");
        victim.enforceValidation(true, true);
        var inputFile = folder.resolve("test.pdf").toFile();
        victim.setTextFromFile(inputFile);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(inputFile.getAbsolutePath(), data.get("fieldIdbrowsableField"));
    }

    @Test
    public void restoreState() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.setId("fieldId");
        Map<String, String> data = new HashMap<>();
        data.put("fieldIdbrowsableField", "/some/file/test.pdf");
        victim.restoreStateFrom(data);
        assertEquals("/some/file/test.pdf", victim.getTextField().getText());
    }
}
