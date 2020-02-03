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

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.ui.dashboard.modules.ModulesDashboardPane;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * @author Andrea Vacondio
 *
 */
@Auto
class ModulesDashboardItem implements DashboardItem {

    static final String ID = "MODULES";

    private ModulesDashboardPane pane;

    @Inject
    ModulesDashboardItem(ModulesDashboardPane pane) {
        this.pane = pane;
    }

    @Override
    public String id() {
        return ID;
    }

    @Override
    public String name() {
        return DefaultI18nContext.getInstance().i18n("Modules");
    }

    @Override
    public Pane pane() {
        return pane;
    }

    @Override
    public Node graphic() {
        return MaterialDesignIconFactory.get().createIcon(MaterialDesignIcon.VIEW_MODULE, "26.0");
    }

    @Override
    public int priority() {
        return -10;
    }

}
