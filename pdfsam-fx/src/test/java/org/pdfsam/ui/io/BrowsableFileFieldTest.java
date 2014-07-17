/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.io.File;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.support.io.FileType;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableFileFieldTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeJavaFxThreadRule fxThread = new InitializeJavaFxThreadRule();

    @Test
    public void setTextFromNullFile() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
        victim.setTextFromFile(null);
        assertEquals(ValidationState.NOT_VALIDATED, victim.getTextField().getValidationState());
    }

    @Test
    public void validExisting() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF);
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("test.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidExisting() throws IOException {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF);
        victim.enforceValidation(true, true);
        File inputFile = folder.newFile("test.oss");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void validNotExisting() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF);
        victim.enforceValidation(false, true);
        File inputFile = new File("ChuckNorris/roundhouse/kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.VALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void invalidNotExisting() {
        BrowsableFileField victim = new BrowsableFileField(FileType.PDF);
        victim.enforceValidation(true, true);
        File inputFile = new File("ChuckNorris/roundhouse/kick.pdf");
        victim.setTextFromFile(inputFile);
        assertEquals(ValidationState.INVALID, victim.getTextField().getValidationState());
        assertEquals(inputFile.getAbsolutePath(), victim.getTextField().getText());
    }

}
