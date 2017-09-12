/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.loadui.testfx.Assertions.verifyThat;

import java.io.File;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableDirectoryFieldTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();

    @Test
    public void setTextFromFile() throws IOException {
        File inputFile = folder.newFile();
        BrowsableDirectoryField victim = new BrowsableDirectoryField();
        victim.setTextFromFile(inputFile);
        assertEquals(inputFile.getParent(), victim.getTextField().getText());
    }

    @Test
    public void setTextFromNullFile() {
        BrowsableDirectoryField victim = new BrowsableDirectoryField();
        victim.setTextFromFile(null);
        verifyThat(victim, v -> isBlank(v.getTextField().getText()));
    }

    @Test
    public void setTextFromDirectory() throws IOException {
        File inputFolder = folder.newFolder();
        BrowsableDirectoryField victim = new BrowsableDirectoryField();
        victim.setTextFromFile(inputFolder);
        assertEquals(inputFolder.getAbsolutePath(), victim.getTextField().getText());
    }

}
