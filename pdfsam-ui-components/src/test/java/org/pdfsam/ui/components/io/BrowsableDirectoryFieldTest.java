/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
import org.pdfsam.test.JavaFxThreadExtension;
import org.testfx.api.FxAssert;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(JavaFxThreadExtension.class)
public class BrowsableDirectoryFieldTest {

    @Test
    public void setTextFromFile(@TempDir Path folder) throws IOException {
        var inputFile = Files.createTempFile(folder, null, null).toFile();
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile(inputFile);
        assertEquals(inputFile.getParent(), victim.getTextField().getText());
    }

    @Test
    public void setTextFromPath(@TempDir Path folder) throws IOException {
        var inputFile = Files.createTempFile(folder, null, null);
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile(inputFile);
        assertEquals(inputFile.getParent().toString(), victim.getTextField().getText());
    }

    @Test
    public void setTextFromNullFile() {
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile((File) null);
        FxAssert.verifyThat(victim, v -> isBlank(v.getTextField().getText()));
    }

    @Test
    public void setTextFromNullPath() {
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile((Path) null);
        FxAssert.verifyThat(victim, v -> isBlank(v.getTextField().getText()));
    }

    @Test
    public void setTextFromDirectory(@TempDir Path folder) throws IOException {
        var inputFolder = Files.createTempDirectory(folder, null).toFile();
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile(inputFolder);
        assertEquals(inputFolder.getAbsolutePath(), victim.getTextField().getText());
    }

    @Test
    public void setTextFromDirectoryPath(@TempDir Path folder) {
        var victim = new BrowsableDirectoryField();
        victim.setTextFromFile(folder);
        assertEquals(folder.toAbsolutePath().toString(), victim.getTextField().getText());
    }

}
