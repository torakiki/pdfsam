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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertEquals;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableFileFieldTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();

    @Test
    public void defaultPromptText() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
        BrowsableFileField victim = new BrowsableFileField(FileType.ALL, OpenType.OPEN);
        assertEquals(DefaultI18nContext.getInstance().i18n("Select a file"), victim.getTextField().getPromptText());
    }

    @Test
    public void setTextFromNullFile() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
        victim.setTextFromFile(null);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
    }

    @Test
    public void validExisting() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("test.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidExisting() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("test.oss");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
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
    public void invalidNotExisting() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        File inputFile = new File("ChuckNorris/roundhouse/kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void validSpecialCharsFolderExisting() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.OPEN);
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("只需要选择需要转换的文件_test.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void saveState() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF, OpenType.SAVE);
        victim.setId("fieldId");
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("test.pdf");
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
