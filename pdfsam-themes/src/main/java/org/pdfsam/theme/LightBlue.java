package org.pdfsam.theme;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/10/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import java.util.ArrayList;
import java.util.List;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
public class LightBlue extends Light {

    @Override
    public String id() {
        return "KLDJHGH3N21A1Z";
    }

    @Override
    public List<String> stylesheets() {
        var css = new ArrayList<>(super.stylesheets());
        css.add("/themes/lightblue/colors.css");
        return css;
    }

    @Override
    public String name() {
        return i18n().tr("Light with blue");
    }

}
