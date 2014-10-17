/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/ott/2014
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
package org.pdfsam;

import static org.pdfsam.support.RequireUtils.requireNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Information about the running version of PDFsam
 * 
 * @author Andrea Vacondio
 *
 */
public class Pdfsam {

    private PdfsamEdition edition;
    private String version;
    private String name;

    public Pdfsam(PdfsamEdition edition, String name, String version) {
        requireNotBlank(name, "Application name cannot be blank");
        requireNotBlank(version, "Application name cannot be blank");
        requireNotNull(edition, "Edition cannot be null");
        this.edition = edition;
        this.version = version;
        this.name = name;
    }

    public PdfsamEdition edition() {
        return edition;
    }

    public String name() {
        return name;
    }

    public String version() {
        return version;
    }
}
