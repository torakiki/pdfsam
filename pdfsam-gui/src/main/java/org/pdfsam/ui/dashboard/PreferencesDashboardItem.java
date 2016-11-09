/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/mag/2014
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
package org.pdfsam.ui.dashboard;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.dashboard.preference.PreferencePane;
import org.sejda.injector.Auto;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * @author Andrea Vacondio
 *
 */
@Auto
class PreferencesDashboardItem implements DashboardItem {

    private PreferencePane pane;

    @Inject
    PreferencesDashboardItem(PreferencePane pane) {
        this.pane = pane;
    }

    @Override
    public String id() {
        return "SETTINGS";
    }

    @Override
    public String name() {
        return DefaultI18nContext.getInstance().i18n("Settings");
    }

    @Override
    public Pane pane() {
        return pane;
    }

    @Override
    public Node graphic() {
        return GlyphsDude.createIcon(MaterialDesignIcon.SETTINGS, "26.0");
    }

    @Override
    public int priority() {
        return -5;
    }

}
