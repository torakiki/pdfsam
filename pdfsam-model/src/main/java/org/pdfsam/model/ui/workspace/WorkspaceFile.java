/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/08/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.model.ui.workspace;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.util.Objects;

/**
 * @author Alessandro Parisi
 */
public class WorkspaceFile {
    private final File file;
    private long hash = -1L;

    public WorkspaceFile(File file) {
        this.file = file;
    }

    private void computeHash() {
        try {
            hash = FileUtils.checksumCRC32(file);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public File file() {
        return file;
    }

    public long hash() {
        if (hash == -1L) {
            computeHash();
        }
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass())
            return false;
        WorkspaceFile that = (WorkspaceFile) o;
        return hash() == that.hash() && Objects.equals(file, that.file);
    }

    @Override
    public int hashCode() {
        return Objects.hash(file, hash());
    }
}
