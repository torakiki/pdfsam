/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ago 2016
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
package org.pdfsam.model.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.tool.ToolInputOutputType;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
public class InputPdfArgumentsLoadRequestTest {

    private InputPdfArgumentsLoadRequest victim = new InputPdfArgumentsLoadRequest();

    @Test
    public void requiredInputTyle(@TempDir Path folder) throws Exception {
        victim.pdfs().add(folder.resolve("test.pdf"));
        assertEquals(ToolInputOutputType.SINGLE_PDF, victim.requiredInputType());
        victim.pdfs().add(folder.resolve("another test.pdf"));
        assertEquals(ToolInputOutputType.MULTIPLE_PDF, victim.requiredInputType());
    }

}
