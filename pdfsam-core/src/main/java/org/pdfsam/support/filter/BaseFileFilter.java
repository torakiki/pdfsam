/*
 * Created on 03-Nov-2007
 * Copyright (C) 2006 by Andrea Vacondio.
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.support.filter;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * Base implementation for a Filter.
 * 
 * @author Andrea Vacondio
 * 
 */
public class BaseFileFilter extends FileFilter implements java.io.FileFilter {

    private FileFilterType type;

    public BaseFileFilter(FileFilterType type) {
        if (type == null) {
            throw new IllegalArgumentException("File filter type cannot be null");
        }
        this.type = type;
    }

    @Override
    public boolean accept(File f) {
        if (f != null) {
            return type.accept(f);
        }
        return false;
    }

    @Override
    public String getDescription() {
        return type.getDescription();
    }

}
