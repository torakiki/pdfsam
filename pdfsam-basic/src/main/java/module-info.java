/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/10/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
module org.pdfsam.basic {
    requires org.apache.commons.lang3;
    requires org.pdfsam.gui;
    requires org.pdfsam.tools.alternatemix;
    requires org.pdfsam.tools.extract;
    requires org.pdfsam.tools.merge;
    requires org.pdfsam.tools.rotate;
    requires org.pdfsam.tools.split;
    requires org.pdfsam.tools.splitbybookmarks;
    requires org.pdfsam.tools.splitbysize;
    requires org.pdfsam.tools.backpages;
    requires jakarta.inject;
    requires javafx.graphics;
    requires org.pdfsam.core;
    requires org.pdfsam.injector;

    exports org.pdfsam.basic;
}