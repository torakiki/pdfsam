/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
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
package org.pdfsam.support.io;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import javafx.scene.input.ClipboardContent;

/**
 * @author Andrea Vacondio
 *
 */
public class ObjectCollectionWriterTest {

    @Rule
    public TemporaryFolder temp = new TemporaryFolder();

    @Test
    public void clipboard() {
        List<String> content = new ArrayList<>();
        content.add("item");
        ClipboardContent clipboard = mock(ClipboardContent.class);
        ObjectCollectionWriter.writeContent(content).to(clipboard);
        verify(clipboard).putString("item" + ObjectCollectionWriter.SEPARATOR);
    }

    @Test
    public void file() throws IOException {
        List<String> content = new ArrayList<>();
        content.add("item");
        File file = temp.newFile();
        ObjectCollectionWriter.writeContent(content).to(file);
        assertTrue(FileUtils.readFileToString(file, Charset.defaultCharset()).contains("item"));
    }
}
