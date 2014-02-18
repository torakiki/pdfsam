/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.quickbar;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.UsageService;

/**
 * Pane holding the most used modules quick access buttons
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class MostUsedModulesPane extends ModulesPane {

    @Inject
    private UsageService usage;

    public MostUsedModulesPane() {
        super(DefaultI18nContext.getInstance().i18n("Most used"));
    }

    @PostConstruct
    private void init() {
        initFor(usage.getMostUsed());
    }
}
