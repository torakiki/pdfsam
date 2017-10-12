/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import java.io.File;

import javafx.scene.input.ClipboardContent;

/**
 * Fluent interface to select the destination of a text write.
 * 
 * @author Andrea Vacondio
 * 
 */
public interface OngoingWrite {

    /**
     * Where the content will be written.
     * 
     * @param file
     */
    void to(File file);

    /**
     * Where the content will be written.
     * 
     * @param clipboard
     */
    void to(ClipboardContent clipboard);
}
