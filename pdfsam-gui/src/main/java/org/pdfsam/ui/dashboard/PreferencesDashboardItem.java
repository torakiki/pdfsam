/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/mag/2014
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
package org.pdfsam.ui.dashboard;

import javax.inject.Inject;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.ui.dashboard.preference.PreferencePane;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * @author Andrea Vacondio
 *
 */
@Auto
public class PreferencesDashboardItem implements DashboardItem {

    public static final String PDFSAM_DISABLE_SETTINGS_DEPRECATED = "org.pdfsam.settings.panel";
    public static final String PDFSAM_DISABLE_SETTINGS = "org.pdfsam.disable.settings.panel";

    public static final String ID = "SETTINGS";
    private PreferencePane pane;

    @Inject
    PreferencesDashboardItem(PreferencePane pane) {
        this.pane = pane;
    }

    @Override
    public String id() {
        return ID;
    }

    @Override
    public String name() {
        return i18n().tr("Settings");
    }

    @Override
    public Pane pane() {
        return pane;
    }

    @Override
    public Node graphic() {
        return MaterialDesignIconFactory.get().createIcon(MaterialDesignIcon.SETTINGS, "26.0");
    }

    @Override
    public int priority() {
        return -5;
    }

    @Override
    public boolean disabled() {
        return Boolean.getBoolean(PDFSAM_DISABLE_SETTINGS_DEPRECATED) || Boolean.getBoolean(PDFSAM_DISABLE_SETTINGS);
    }
}
