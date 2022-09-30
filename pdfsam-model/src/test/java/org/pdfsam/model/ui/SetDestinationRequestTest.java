/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/lug/2014
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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
public class SetDestinationRequestTest {

    @Test
    public void nullArg() {
        assertThrows(IllegalArgumentException.class, () -> SetDestinationRequest.requestDestination(null, "module"));
    }

    @Test
    public void fallback(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null).toFile();
        assertTrue(SetDestinationRequest.requestFallbackDestination(file, "module").fallback());
        assertFalse(SetDestinationRequest.requestDestination(file, "module").fallback());
    }

    @Test
    public void footprint(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null).toFile();
        SetDestinationRequest victim = SetDestinationRequest.requestFallbackDestination(file, "module");
        assertEquals(new File(file.getParent(), "PDFsam_module.pdf").getAbsolutePath(),
                victim.footprint().getAbsolutePath());
    }
}
